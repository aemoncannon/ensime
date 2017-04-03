// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{ Failure, Success }

import akka.actor._
import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2._
import org.ensime.api._
import org.ensime.indexer.database._
import org.ensime.indexer.database.DatabaseService._
import org.ensime.util.file._
import org.ensime.util.fileobject._
import org.ensime.vfs._

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
) extends FileChangeListener with SLF4JLogging {
  import ExecutionContext.Implicits.global

  private[indexer] def isUserFile(file: FileName): Boolean = {
    (config.allTargets map (vfs.vfile)) exists (file isAncestor _.getName)
  }

  private val QUERY_TIMEOUT = 30 seconds

  /**
   * Changelog:
   *
   * 2.0.3 - added JDI source information
   *
   * 2.0.2 - bump lucene and h2 versions
   *
   * 2.0.1 - change the lucene analyser
   *
   * 2.0 - upgrade Lucene, format not backwards compatible.
   *
   * 1.4 - remove redundant descriptors, doh!
   *
   * 1.3 - methods include descriptors in (now unique) FQNs
   *
   * 1.2 - added foreign key to FqnSymbols.file with cascade delete
   *
   * 1.0 - reverted index due to negative impact to startup time. The
   *       workaround to large scale deletions is to just nuke the
   *       .ensime_cache.
   *
   * 1.1 - added index to FileCheck.file to speed up delete.
   *
   * 1.0 - initial schema
   */
  private val version = "2.0.3"

  private[indexer] val index = new IndexService((config.cacheDir / ("index-" + version)).toPath)
  private val db = new DatabaseService(config.cacheDir / ("sql-" + version))

  private def scan(f: FileObject) = f.findFiles(ClassfileSelector) match {
    case null => Nil
    case res => res.toList
  }

  /**
   * Indexes everything, making best endeavours to avoid scanning what
   * is unnecessary (e.g. we already know that a jar or classfile has
   * been indexed).
   *
   * @return the number of rows (removed, indexed) from the database.
   */
  def refresh(): Future[(Int, Int)] = {
    // it is much faster during startup to obtain the full list of
    // known files from the DB then and check against the disk, than
    // check each file against DatabaseService.outOfDate
    def findStaleFileChecks(checks: Seq[FileCheck]): List[FileCheck] = {
      log.debug("findStaleFileChecks")
      for {
        check <- checks
        if !check.file.exists || check.changed
      } yield check
    }.toList

    // delete the stale data before adding anything new
    // returns number of rows deleted
    def deleteReferences(checks: List[FileCheck]): Future[Int] = {
      log.debug(s"removing ${checks.size} stale files from the index")
      deleteInBatches(checks.map(_.file))
    }

    // a snapshot of everything that we want to index
    def findBases(): Set[FileObject] = {
      config.modules.flatMap {
        case (name, m) =>
          m.targets.flatMap {
            case d if !d.exists() => Nil
            case d if d.isJar => List(vfs.vfile(d))
            case d => scan(vfs.vfile(d))
          } ::: m.testTargets.flatMap {
            case d if !d.exists() => Nil
            case d if d.isJar => List(vfs.vfile(d))
            case d => scan(vfs.vfile(d))
          } :::
            m.compileJars.map(vfs.vfile) ::: m.testJars.map(vfs.vfile)
      }
    }.toSet ++ config.javaLibs.map(vfs.vfile)

    def indexBase(base: FileObject, fileCheck: Option[FileCheck]): Future[Int] = {
      val outOfDate = fileCheck.map(_.changed).getOrElse(true)
      if (!outOfDate) Future.successful(0)
      else {
        val boost = isUserFile(base.getName())
        val check = FileCheck(base)
        val indexed = extractSymbolsFromClassOrJar(base).flatMap(persist(check, _, commitIndex = false, boost = boost))
        indexed
      }
    }

    // index all the given bases and return number of rows written
    def indexBases(bases: Set[FileObject], checks: Seq[FileCheck]): Future[Int] = {
      log.debug("Indexing bases...")
      val checksLookup: Map[String, FileCheck] = checks.map(check => (check.filename -> check)).toMap
      val basesWithChecks: Set[(FileObject, Option[FileCheck])] = bases.map { base =>
        (base, checksLookup.get(base.uriString))
      }
      Future.sequence(basesWithChecks.map { case (file, check) => indexBase(file, check) }).map(_.sum)
    }

    def commitIndex(): Future[Unit] = {
      log.debug("committing index to disk...")
      val i = index.commit()
      val g = db.commit()
      for {
        _ <- i
        _ <- g
      } yield {
        log.debug("...done committing index")
      }
    }

    // chain together all the future tasks
    for {
      checks <- db.knownFiles()
      stale = findStaleFileChecks(checks)
      deletes <- deleteReferences(stale)
      bases = findBases()
      added <- indexBases(bases, checks)
      _ <- commitIndex()
    } yield (deletes, added)
  }

  def refreshResolver(): Unit = resolver.update()

  def persist(check: FileCheck, symbols: List[FqnSymbol], commitIndex: Boolean, boost: Boolean): Future[Int] = {
    val iwork = index.persist(check, symbols, commitIndex, boost)
    val dwork = db.persist(check, symbols)

    for {
      _ <- iwork
      inserts <- dwork
    } yield inserts
  }

  def extractSymbolsFromClassOrJar(file: FileObject): Future[List[FqnSymbol]] = {
    def global: ExecutionContext = null // detach the global implicit
    val ec = actorSystem.dispatchers.lookup("akka.search-service-dispatcher")

    Future {
      blocking {
        file match {
          case classfile if classfile.getName.getExtension == "class" =>
            // too noisy to log
            val check = FileCheck(classfile)
            try extractSymbols(classfile, classfile)
            finally classfile.close()
          case jar =>
            log.debug(s"indexing $jar")
            val check = FileCheck(jar)
            val vJar = vfs.vjar(jar.asLocalFile)
            try scan(vJar) flatMap (extractSymbols(jar, _))
            finally vfs.nuke(vJar)
        }
      }
    }(ec)
  }

  private val blacklist = Set("sun/", "sunw/", "com/sun/")
  private val ignore = Set("$$", "$worker$")
  private def extractSymbols(container: FileObject, f: FileObject): List[FqnSymbol] = {
    f.pathWithinArchive match {
      case Some(relative) if blacklist.exists(relative.startsWith) => Nil
      case _ =>
        val name = container.uriString
        val path = f.uriString
        val indexer = new ClassfileIndexer(f)
        val (clazz, refs) = indexer.indexClassfile()

        val depickler = new ClassfileDepickler(f)

        val source = resolver.resolve(clazz.name.pack, clazz.source)
        val sourceUri = source.map(_.uriString)

        val jdi = source.map { src =>
          val pkg = clazz.name.pack.path.mkString("/")
          s"$pkg/${src.getName.getBaseName}"
        }

        if (clazz.access != Public) Nil
        else {
          FqnSymbol(None, name, path, clazz.name.fqnString, None, sourceUri, clazz.source.line, None, jdi) ::
            clazz.methods.toList.filter(_.access == Public).map { method =>
              FqnSymbol(None, name, path, method.name.fqnString, None, sourceUri, method.line)
            } ::: clazz.fields.toList.filter(_.access == Public).map { field =>
              val internal = field.clazz.internalString
              FqnSymbol(None, name, path, field.name.fqnString, Some(internal), sourceUri, clazz.source.line)
            } ::: depickler.getTypeAliases.toList.filter(_.access == Public).filterNot(_.fqn.contains("<refinement>")).map { rawType =>
              // this is a hack, we shouldn't be storing Scala names in the JVM name space
              // in particular, it creates fqn names that clash with the above ones
              FqnSymbol(None, name, path, rawType.fqn, None, sourceUri, None)
            }
        }
    }
  }.filterNot(sym => ignore.exists(sym.fqn.contains))

  /** free-form search for classes */
  def searchClasses(query: String, max: Int): List[FqnSymbol] = {
    val fqns = Await.result(index.searchClasses(query, max), QUERY_TIMEOUT)
    Await.result(db.find(fqns), QUERY_TIMEOUT) take max
  }

  /** free-form search for classes and methods */
  def searchClassesMethods(terms: List[String], max: Int): List[FqnSymbol] = {
    val fqns = Await.result(index.searchClassesMethods(terms, max), QUERY_TIMEOUT)
    Await.result(db.find(fqns), QUERY_TIMEOUT) take max
  }

  /** only for exact fqns */
  def findUnique(fqn: String): Option[FqnSymbol] = Await.result(db.find(fqn), QUERY_TIMEOUT)

  def findClasses(file: EnsimeFile): Seq[FqnSymbol] = Await.result(db.findClasses(file), QUERY_TIMEOUT)
  def findClasses(jdi: String): Seq[FqnSymbol] = Await.result(db.findClasses(jdi), QUERY_TIMEOUT)

  /* DELETE then INSERT in H2 is ridiculously slow, so we put all modifications
   * into a blocking queue and dedicate a thread to block on draining the queue.
   * This has the effect that we always react to a single change on disc but we
   * will work through backlogs in bulk.
   *
   * We always do a DELETE, even if the entries are new, but only INSERT if
   * the list of symbols is non-empty.
   */

  val backlogActor = actorSystem.actorOf(Props(new IndexingQueueActor(this)), "ClassfileIndexer")

  // deletion in both Lucene and H2 is really slow, batching helps
  def deleteInBatches(
    files: List[FileObject],
    batchSize: Int = 1000
  ): Future[Int] = {
    val removing = files.grouped(batchSize).map(delete)
    Future.sequence(removing).map(_.sum)
  }

  // returns number of rows removed
  def delete(files: List[FileObject]): Future[Int] = {
    // this doesn't speed up Lucene deletes, but it means that we
    // don't wait for Lucene before starting the H2 deletions.
    val iwork = index.remove(files)
    val dwork = db.removeFiles(files)

    for {
      _ <- iwork
      removals <- dwork
    } yield removals
  }

  def fileChanged(f: FileObject): Unit = backlogActor ! IndexFile(f)
  def fileRemoved(f: FileObject): Unit = fileChanged(f)
  def fileAdded(f: FileObject): Unit = fileChanged(f)

  def shutdown(): Future[Unit] = Future.sequence {
    List(db.shutdown(), index.shutdown())
  } map (_ => ())
}

final case class IndexFile(f: FileObject)

class IndexingQueueActor(searchService: SearchService) extends Actor with ActorLogging {
  import context.system

  import scala.concurrent.duration._

  case object Process

  // De-dupes files that have been updated since we were last told to
  // index them. No need to aggregate values: the latest wins. Key is
  // the URI because FileObject doesn't implement equals
  var todo = Map.empty[String, FileObject]

  // debounce and give us a chance to batch (which is *much* faster)
  var worker: Cancellable = _

  private val advice = "If the problem persists, you may need to restart ensime."

  private def debounce(): Unit = {
    Option(worker).foreach(_.cancel())
    import context.dispatcher
    worker = system.scheduler.scheduleOnce(5 seconds, self, Process)
  }

  override def receive: Receive = {
    case IndexFile(f) =>
      todo += f.uriString -> f
      debounce()

    case Process if todo.isEmpty => // nothing to do

    case Process =>
      val (batch, remaining) = todo.splitAt(500)
      todo = remaining
      if (remaining.nonEmpty)
        debounce()

      import ExecutionContext.Implicits.global

      log.debug(s"Indexing ${batch.size} files")

      def retry(): Unit = {
        batch.foreach(self !)
      }

      Future.sequence(batch.map {
        case (url, f) =>
          val filename = f.getName.getPath
          // I don't trust VFS's f.exists()
          if (!File(filename).exists()) {
            Future {
              f -> Nil
            }
          } else searchService.extractSymbolsFromClassOrJar(f).map(f -> )
      }).onComplete {
        case Failure(t) =>
          log.error(t, s"failed to index batch of ${batch.size} files. $advice")
          retry()
        case Success(indexed) =>
          searchService.delete(indexed.map(_._1)(collection.breakOut)).onComplete {
            case Failure(t) =>
              log.error(t, s"failed to remove stale entries in ${batch.size} files. $advice")
              retry()
            case Success(_) => indexed.foreach {
              case (file, syms) =>
                val boost = searchService.isUserFile(file.getName)
                val persisting = searchService.persist(FileCheck(file), syms, commitIndex = true, boost = boost)

                persisting.onComplete {
                  case Failure(t) =>
                    log.error(t, s"failed to persist entries in $file. $advice")
                    retry()
                  case Success(_) =>
                }
            }
          }

      }
  }

}
