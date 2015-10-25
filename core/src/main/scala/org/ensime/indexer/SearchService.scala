package org.ensime.indexer

import java.sql.SQLException

import akka.actor._
import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2._
import org.ensime.api._
import org.ensime.indexer.DatabaseService._
import org.ensime.util.file._

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

/**
 * Provides methods to perform ENSIME-specific indexing tasks,
 * receives events that require an index update, and provides
 * searches against the index.
 *
 * We have an H2 database for storing relational information
 * and Lucene for advanced indexing.
 */
class SearchService(
  config: EnsimeConfig,
  resolver: SourceResolver
)(
  implicit
  actorSystem: ActorSystem,
  vfs: EnsimeVFS
) extends ClassfileIndexer
    with ClassfileListener
    with SLF4JLogging {

  private val QUERY_TIMEOUT = 30 seconds
  private val version = "1.0"

  private val index = new IndexService(config.cacheDir / ("index-" + version))
  private val db = new DatabaseService(config.cacheDir / ("sql-" + version))

  implicit val workerEC = actorSystem.dispatchers.lookup("akka.search-service-dispatcher")

  // FIXME: apologies, this is pretty messy. We should move to an
  // actor based system for all of the persisting instead of this
  // hybrid approach.

  /**
   * Indexes everything, making best endeavours to avoid scanning what
   * is unnecessary (e.g. we already know that a jar or classfile has
   * been indexed).
   *
   * The decision of what will be indexed is performed syncronously,
   * as is the removal of stale data, but the itself itself is
   * performed asyncronously.
   *
   * @return the number of files estimated to be (removed, indexed)
   *         from the index and database. This is only an estimate
   *         because we may not have TODO
   */
  def refresh(): Future[(Int, Int)] = {
    def scan(f: FileObject) = f.findFiles(EnsimeVFS.ClassfileSelector) match {
      case null => Nil
      case res => res.toList
    }

    // TODO visibility test/main and which module is viewed (a Lucene concern, not H2)

    val jarUris = config.allJars.map(vfs.vfile).map(_.getName.getURI)

    for {
      // remove stale entries: must be before index or INSERT/DELETE races
      stale <- db.knownFiles().map { knownFiles =>
        for {
          known <- knownFiles
          f = known.file
          name = f.getName.getURI
          if !f.exists || known.changed ||
            (name.endsWith(".jar") && !jarUris(name))
        } yield f
      }

      _ = {
        log.info(s"removing ${stale.size} stale files from the index")
        if (log.isTraceEnabled)
          log.trace(s"STALE = $stale")
      }

      _ <- Future.sequence(
        // individual DELETEs in H2 are really slow
        stale.grouped(1000).map(delete)
      )

      // start indexing after all deletes have completed (not pretty)

      bases = {
        config.modules.flatMap {
          case (name, m) =>
            m.targetDirs.flatMap { d => scan(vfs.vfile(d)) } ::: m.testTargetDirs.flatMap { d => scan(vfs.vfile(d)) } :::
              m.compileJars.map(vfs.vfile) ::: m.testJars.map(vfs.vfile)
        }
      }.toSet ++ config.javaLibs.map(vfs.vfile)

      basesWithOutOfDateInfo <- Future.sequence(bases.map(b => db.outOfDate(b).map((b, _))))
      persisted <- Future.sequence(
        basesWithOutOfDateInfo.collect { // is there a simpler way to do this, rather than collect after sequence?
          case (base, outOfDate) if outOfDate => base
        }.map {
          case classfile if classfile.getName.getExtension == "class" => Future[Future[Unit]] {
            val check = FileCheck(classfile)
            val symbols = extractSymbols(classfile, classfile)
            persist(check, symbols)
          }.flatMap(identity)

          case jar => Future[Future[Unit]] {
            try {
              log.debug(s"indexing $jar")
              val check = FileCheck(jar)
              val symbols = scan(vfs.vjar(jar)) flatMap (extractSymbols(jar, _))
              persist(check, symbols)
            } catch {
              case e: Exception =>
                log.error(s"Failed to index $jar", e)
                Future.successful(())
            }
          }.flatMap(identity)
        }
      )

      _ = {
        // delayed commits speedup initial indexing time
        log.debug("committing index to disk...")
        index.commit()
        log.debug("...done committing index")
      }

    } yield (stale.size, persisted.size)
  }

  def refreshResolver(): Unit = resolver.update()

  def persist(check: FileCheck, symbols: List[FqnSymbol]): Future[Unit] = try {
    index.persist(check, symbols)
    db.persist(check, symbols).map(_ => ())
  } catch {
    case e: SQLException =>
      // likely a timing issue or corner-case dupe FQNs
      log.warn(s"failed to insert ${symbols.size} symbols for ${check.file} ${e.getClass}: ${e.getMessage}")
      Future.successful(())
  }

  private val blacklist = Set("sun/", "sunw/", "com/sun/")
  private val ignore = Set("$$anon$", "$$anonfun$", "$worker$")
  import org.ensime.util.RichFileObject._
  private def extractSymbols(container: FileObject, f: FileObject): List[FqnSymbol] = {
    f.pathWithinArchive match {
      case Some(relative) if blacklist.exists(relative.startsWith) => Nil
      case _ =>
        val name = container.getName.getURI
        val path = f.getName.getURI
        val (clazz, refs) = indexClassfile(f)

        val depickler = new ClassfileDepickler(f)

        val source = resolver.resolve(clazz.name.pack, clazz.source)
        val sourceUri = source.map(_.getName.getURI)

        // TODO: other types of visibility when we get more sophisticated
        if (clazz.access != Public) Nil
        else FqnSymbol(None, name, path, clazz.name.fqnString, None, None, sourceUri, clazz.source.line) ::
          clazz.methods.toList.filter(_.access == Public).map { method =>
            val descriptor = method.descriptor.descriptorString
            FqnSymbol(None, name, path, method.name.fqnString, Some(descriptor), None, sourceUri, method.line)
          } ::: clazz.fields.toList.filter(_.access == Public).map { field =>
            val internal = field.clazz.internalString
            FqnSymbol(None, name, path, field.name.fqnString, None, Some(internal), sourceUri, clazz.source.line)
          } ::: depickler.getTypeAliases.toList.filter(_.access == Public).map { rawType =>
            FqnSymbol(None, name, path, rawType.fqnString, None, None, sourceUri, None)
          }

    }
  }.filterNot(sym => ignore.exists(sym.fqn.contains))

  // TODO: provide context (user's current module and main/test)
  /** free-form search for classes */
  def searchClasses(query: String, max: Int): List[FqnSymbol] = {
    val fqns = index.searchClasses(query, max)
    Await.result(db.find(fqns), QUERY_TIMEOUT) take max
  }

  /** free-form search for classes and methods */
  def searchClassesMethods(terms: List[String], max: Int): List[FqnSymbol] = {
    val fqns = index.searchClassesMethods(terms, max)
    Await.result(db.find(fqns), QUERY_TIMEOUT) take max
  }

  /** only for exact fqns */
  def findUnique(fqn: String): Option[FqnSymbol] = Await.result(db.find(fqn), QUERY_TIMEOUT)

  /* DELETE then INSERT in H2 is ridiculously slow, so we put all modifications
   * into a blocking queue and dedicate a thread to block on draining the queue.
   * This has the effect that we always react to a single change on disc but we
   * will work through backlogs in bulk.
   *
   * We always do a DELETE, even if the entries are new, but only INSERT if
   * the list of symbols is non-empty.
   */

  val backlogActor = actorSystem.actorOf(Props(new IndexingQueueActor(this)), "ClassfileIndexer")

  def delete(files: List[FileObject]): Future[Int] = {
    index.remove(files)
    db.removeFiles(files)
  }

  def classfileAdded(f: FileObject): Unit = classfileChanged(f)

  def classfileRemoved(f: FileObject): Unit = {
    backlogActor ! FileUpdate(f, Nil)
  }

  def classfileChanged(f: FileObject): Unit = Future {
    val symbols = extractSymbols(f, f)
    backlogActor ! FileUpdate(f, symbols)
  }(workerEC)

  def shutdown(): Future[Unit] = {
    db.shutdown()
  }
}

case class FileUpdate(
  fileObject: FileObject,
  symbolList: List[FqnSymbol]
)

class IndexingQueueActor(searchService: SearchService) extends Actor with ActorLogging {
  import context.system

  import scala.concurrent.duration._

  case object Process

  // De-dupes files that have been updated since we were last told to
  // index them. No need to aggregate values: the latest wins.
  var todo = Map.empty[FileObject, List[FqnSymbol]]

  // debounce and give us a chance to batch (which is *much* faster)
  var worker: Cancellable = _

  private def debounce(): Unit = {
    Option(worker).foreach(_.cancel())
    import context.dispatcher
    worker = system.scheduler.scheduleOnce(5 seconds, self, Process)
  }

  override def receive: Receive = {
    case FileUpdate(fo, syms) =>
      todo += fo -> syms
      debounce()

    case Process if todo.isEmpty => // nothing to do

    case Process =>
      val (batch, remaining) = todo.splitAt(500)
      todo = remaining

      log.debug(s"Indexing ${batch.size} classfiles")

      // blocks the actor thread intentionally -- this is real work
      // and the underlying I/O implementation is blocking. Give me an
      // async SQL database and we can talk...
      //
      // UPDATE 2015-10-24: Slick no longer blocking. This can most likely be fixed now
      // (however the author of the above comment imagined)

      {
        import searchService.workerEC // TODO: check the right EC is used here
        // batch the deletion (big bottleneck)
        Await.ready(
          for {
            _ <- searchService.delete(batch.keys.toList)
            _ <- Future.sequence(
              // opportunity to do more batching here
              batch.collect {
                case (file, syms) if syms.nonEmpty =>
                  searchService.persist(FileCheck(file), syms)
              }
            )
          } yield (),
          Duration.Inf
        )
      }
  }

}
