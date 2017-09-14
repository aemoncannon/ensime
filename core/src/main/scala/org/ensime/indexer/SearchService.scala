// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import org.ensime.util.Debouncer
import scala.collection.{ mutable, Map, Set }
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{ Failure, Properties, Success }

import akka.actor._
import akka.event.slf4j.SLF4JLogging
import org.ensime.api._
import org.ensime.config.EnsimeConfigProtocol
import org.ensime.config.richconfig._
import org.ensime.indexer.graph._
import org.ensime.util.ensimefile._
import org.ensime.util.path._
import org.ensime.util.map._

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
    serverConfig: EnsimeServerConfig
) extends FileChangeListener with SLF4JLogging {
  import SearchService._
  import ExecutionContext.Implicits.global // not used for heavy lifting (indexing, graph or lucene)

  private[indexer] val allTargets = config.targets.map(_.toPath)

  private[indexer] def isUserFile(file: EnsimeFile): Boolean = {
    val rootPath = file match {
      case RawFile(path) => path
      case ArchiveFile(root, entry) => root
    }
    allTargets.exists(rootPath.startsWith)
  }

  private val QUERY_TIMEOUT = 30 seconds

  /**
   * Changelog:
   *
   * 2.0.4 - find usages and show implementations using Indexer
   *
   * 2.0.3g - graphpocalypse
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
   * 1.3g - use a graph database
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
  private val version = "2.0.4"

  private[indexer] val index = new IndexService(config.cacheDir.path / ("index-" + version))
  private val db = new GraphService((config.cacheDir.path / ("graph-" + version)).toFile)
  val noReverseLookups: Boolean = Properties.propOrFalse("ensime.index.no.reverse.lookups")

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
    def findBases(): (Set[EnsimeFile], Map[EnsimeFile, Set[EnsimeFile]]) = {
      //TODO: jarFiles is List[RawFile], why not ArchiveFile?
      val (jarFiles, dirs) = config.projects.flatMap {
        case m =>
          m.targets.filter(_.exists).toList :::
            m.libraryJars
      }.partition(_.isJar)
      val grouped = dirs.map(d => d.scanGrouped).fold(Map.empty[EnsimeFile, Set[EnsimeFile]])(_ merge _)
      val jars: Set[EnsimeFile] =
        (jarFiles ++ EnsimeConfigProtocol.javaRunTime(config).map(p => RawFile(p))).toSet
      (jars, grouped)
    }

    def indexBase(
      base: EnsimeFile,
      fileCheck: Option[FileCheck],
      grouped: Map[EnsimeFile, Set[EnsimeFile]]
    ): Future[Int] = {
      val outOfDate = fileCheck.forall(_.changed)
      if (!outOfDate || !base.exists) Future.successful(0)
      else {
        val boost = isUserFile(base)
        val indexed = extractSymbolsFromClassOrJar(base, grouped).flatMap(persist(_, commitIndex = false, boost = boost))
        indexed.onComplete { _ =>
          if (base.isJar) {
            log.debug(s"finished indexing $base")
          }
        }
        indexed
      }
    }

    // TODO: private?
    // index all the given bases and return number of rows written
    def indexBases(files: (Set[EnsimeFile], Map[EnsimeFile, Set[EnsimeFile]]), checks: Seq[FileCheck]): Future[Int] = {
      val (jars, classFiles) = files
      log.debug("Indexing bases...")

      val checksLookup: Map[String, FileCheck] = checks.map(check => (check.filename -> check)).toMap
      val jarsWithChecks = jars.map(jar => (jar, checksLookup.get(jar.uriString)))

      val basesWithChecks: Seq[(EnsimeFile, Option[FileCheck])] = classFiles.map {
        case (outerClassFile, _) =>
          (outerClassFile, checksLookup.get(outerClassFile.uriString))
      }(collection.breakOut)

      (jarsWithChecks ++ basesWithChecks).grouped(serverConfig.indexBatchSize).foldLeft(Future.successful(0)) {
        (indexedCount, batch) =>
          for {
            c <- indexedCount
            b <- Future.sequence {
              batch.map { case (file, check) => indexBase(file, check, classFiles) }
            }.map(_.sum)
          } yield c + b
      }
    }

    // chain together all the future tasks
    for {
      checks <- db.knownFiles()
      stale = findStaleFileChecks(checks)
      deletes <- deleteReferences(stale)
      bases = findBases()
      added <- indexBases(bases, checks)
      _ <- index.commit()
    } yield (deletes, added)
  }

  def refreshResolver(): Unit = resolver.update()

  def persist(symbols: List[SourceSymbolInfo], commitIndex: Boolean, boost: Boolean): Future[Int] = {
    val iwork = index.persist(symbols, commitIndex, boost)
    val dwork = db.persist(symbols)

    for {
      _ <- iwork
      inserts <- dwork
    } yield inserts
  }

  def extractSymbolsFromClassOrJar(
    file: EnsimeFile,
    grouped: Map[EnsimeFile, Set[EnsimeFile]]
  ): Future[List[SourceSymbolInfo]] = {
    def global: ExecutionContext = null // detach the global implicit
    val ec = actorSystem.dispatchers.lookup("akka.search-service-dispatcher")

    Future {
      blocking {
        file match {
          case classfile: RawFile if classfile.isClass =>
            // too noisy to log
            val files = grouped(classfile)
            //try
            extractSymbols(classfile, files, classfile)
          //TODO: Add java.nio closing logic
          //finally { files.foreach(_.close()); classfile.close() }
          case jar: RawFile =>
            log.debug(s"indexing ${jar.path}")
            //try {
            val res = (jar.scanGrouped.flatMap {
              case (root, files) => extractSymbols(jar, files, root)
            }).toList
            res
          //} finally { vfs.nuke(vJar) }
          case ArchiveFile(root, entry) => ???
        }
      }
    }(ec)
  }

  private val blacklist = Set("sun/", "sunw/", "com/sun/")
  private val ignore = Set("$$", "$worker$")

  private def extractSymbols(
    container: EnsimeFile,
    files: collection.Set[EnsimeFile],
    rootClassFile: EnsimeFile
  ): List[SourceSymbolInfo] = {
    def getInternalRefs(isUserFile: Boolean, s: RawSymbol): List[FullyQualifiedReference] = if (isUserFile && !noReverseLookups) s.internalRefs else List.empty

    val (fs, path) = rootClassFile.pathWithOpenFs()
    val depickler = new ClassfileDepickler(path)
    val scalapClasses = depickler.getClasses
    fs.map(_.close())

    val res: List[SourceSymbolInfo] = files.flatMap {
      case f if f.pathWithinArchive.exists(
        relative => blacklist.exists(relative.startsWith)
      ) => List(EmptySourceSymbolInfo(FileCheck(container)))
      case f =>
        val path = f.uriString
        val file = if (path.startsWith("jar") || path.startsWith("zip")) {
          FileCheck(container)
        } else FileCheck(f)
        val indexer = new ClassfileIndexer(f)
        val clazz = indexer.indexClassfile()

        val userFile = isUserFile(f)
        val source = resolver.resolve(clazz.name.pack, clazz.source)

        val sourceUri = source.map(_.uriString)

        val jdi = source.map { src =>
          val pkg = clazz.name.pack.path.mkString("/")
          s"$pkg/${src.path.getFileName}"
        }

        val scalapClassInfo = scalapClasses.get(clazz.name.fqnString)

        scalapClassInfo match {
          case _ if clazz.access != Public => List(EmptySourceSymbolInfo(file))
          case _ if ignore.exists(clazz.fqn.contains) => Nil
          case Some(scalapSymbol) =>
            val classInfo = ClassSymbolInfo(file, path, sourceUri, getInternalRefs(userFile, clazz), clazz, Some(scalapSymbol), jdi)

            val fields = clazz.fields.map(f =>
              FieldSymbolInfo(file, sourceUri, getInternalRefs(userFile, f), f, scalapSymbol.fields.get(f.fqn)))

            val methods = clazz.methods.groupBy(_.name.name).flatMap {
              case (methodName, overloads) =>
                val scalapMethods = scalapSymbol.methods.get(methodName)
                overloads.iterator.zipWithIndex.map {
                  case (m, i) =>
                    val scalap = scalapMethods.fold(Option.empty[RawScalapMethod])(seq =>
                      if (seq.length <= i) None else Some(seq(i)))
                    MethodSymbolInfo(file, sourceUri, getInternalRefs(userFile, m), m, scalap)
                }
            }

            val aliases = scalapSymbol.typeAliases.valuesIterator.map(alias =>
              TypeAliasSymbolInfo(file, sourceUri, alias)).toList

            classInfo :: fields ::: methods.toList ::: aliases
          case None =>
            val cl = ClassSymbolInfo(file, path, sourceUri, getInternalRefs(userFile, clazz), clazz, None, jdi)
            val methods: List[MethodSymbolInfo] = clazz.methods.map(m =>
              MethodSymbolInfo(file, sourceUri, getInternalRefs(userFile, m), m, None))(collection.breakOut)
            val fields = clazz.fields.map(f =>
              FieldSymbolInfo(file, sourceUri, getInternalRefs(userFile, f), f, None))
            cl :: methods ::: fields
        }
    }(collection.breakOut)
    res.filterNot(sym => ignore.exists(sym.fqn.contains)).sortWith {
      case (cl1: ClassSymbolInfo, cl2: ClassSymbolInfo) => cl1.fqn < cl2.fqn
      case (cl: ClassSymbolInfo, _) => true
      case _ => false
    }
  }

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

  /** returns hierarchy of a type identified by fqn */
  def getTypeHierarchy(fqn: String, hierarchyType: Hierarchy.Direction, levels: Option[Int] = None): Future[Option[Hierarchy]] = db.getClassHierarchy(fqn, hierarchyType, levels)

  /** returns locations where given fqn is referred*/
  def findUsageLocations(fqn: String): Future[Iterable[UsageLocation]] = db.findUsageLocations(fqn)

  /** returns FqnSymbols where given fqn is referred*/
  def findUsages(fqn: String): Future[Iterable[FqnSymbol]] = db.findUsages(fqn)

  // blocking badness
  def findClasses(file: EnsimeFile): Seq[ClassDef] = Await.result(db.findClasses(file), QUERY_TIMEOUT)
  def findClasses(jdi: String): Seq[ClassDef] = Await.result(db.findClasses(jdi), QUERY_TIMEOUT)

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
    files: List[EnsimeFile],
    batchSize: Int = 1000
  ): Future[Int] = {
    val removing = files.grouped(batchSize).map(delete)
    Future.sequence(removing).map(_.sum)
  }

  // returns number of rows removed
  def delete(files: List[EnsimeFile]): Future[Int] = {
    // this doesn't speed up Lucene deletes, but it means that we
    // don't wait for Lucene before starting the H2 deletions.
    val iwork = index.remove(files)
    val dwork = db.removeFiles(files)

    for {
      _ <- iwork
      removals <- dwork
    } yield removals
  }

  def fileChanged(f: RawFile): Unit = backlogActor ! IndexFile(f)
  def fileRemoved(f: RawFile): Unit = fileChanged(f)
  def fileAdded(f: RawFile): Unit = fileChanged(f)

  def shutdown(): Future[Unit] = Future.sequence {
    List(db.shutdown(), index.shutdown())
  } map (_ => ())
}

object SearchService {
  sealed trait SourceSymbolInfo {
    def file: FileCheck
    def fqn: String
    def internalRefs: List[FullyQualifiedReference]
    def scalapSymbol: Option[RawScalapSymbol]
  }

  final case class EmptySourceSymbolInfo(
      file: FileCheck
  ) extends SourceSymbolInfo {
    override def fqn: String = ""
    override def internalRefs: List[FullyQualifiedReference] = List.empty
    override def scalapSymbol: Option[RawScalapSymbol] = None
  }

  final case class ClassSymbolInfo(
      file: FileCheck,
      path: String,
      source: Option[String],
      internalRefs: List[FullyQualifiedReference],
      bytecodeSymbol: RawClassfile,
      scalapSymbol: Option[RawScalapClass],
      jdi: Option[String]
  ) extends SourceSymbolInfo {
    override def fqn: String = bytecodeSymbol.fqn
  }

  final case class MethodSymbolInfo(
      file: FileCheck,
      source: Option[String],
      internalRefs: List[FullyQualifiedReference],
      bytecodeSymbol: RawMethod,
      scalapSymbol: Option[RawScalapMethod]
  ) extends SourceSymbolInfo {
    override def fqn: String = bytecodeSymbol.fqn
  }

  final case class FieldSymbolInfo(
      file: FileCheck,
      source: Option[String],
      internalRefs: List[FullyQualifiedReference],
      bytecodeSymbol: RawField,
      scalapSymbol: Option[RawScalapField]
  ) extends SourceSymbolInfo {
    override def fqn: String = bytecodeSymbol.fqn
  }

  final case class TypeAliasSymbolInfo(
      file: FileCheck,
      source: Option[String],
      t: RawType
  ) extends SourceSymbolInfo {
    override def scalapSymbol: Option[RawScalapSymbol] = Some(t)
    override def fqn: String = t.javaName.fqnString
    override def internalRefs: List[FullyQualifiedReference] = List.empty
  }
}

final case class IndexFile(f: EnsimeFile)

class IndexingQueueActor(searchService: SearchService) extends Actor with ActorLogging {
  import scala.concurrent.duration._

  case object Process

  // De-dupes files that have been updated since we were last told to
  // index them. No need to aggregate values: the latest wins. Key is
  // the URI because FileObject doesn't implement equals
  private val todo = new mutable.HashMap[EnsimeFile, mutable.Set[EnsimeFile]] with mutable.MultiMap[EnsimeFile, EnsimeFile]

  private val advice = "If the problem persists, you may need to restart ensime."

  val processDebounce = Debouncer.forActor(self, Process, delay = 5.seconds, maxDelay = 1.hour)

  override def receive: Receive = {
    case IndexFile(f) =>
      val topLevelClassFile = f match {
        case jar if jar.isJar => jar
        case classFile: RawFile => classFile.getTopLevelClassFile
      }
      todo.addBinding(topLevelClassFile, topLevelClassFile)
      processDebounce.call()

    case Process if todo.isEmpty => // nothing to do

    case Process =>
      val batch = todo.take(250)
      batch.keys.foreach(todo.remove)
      if (todo.nonEmpty)
        processDebounce.call()

      import ExecutionContext.Implicits.global

      log.debug(s"Indexing ${batch.size} groups of files")

      def retry(): Unit = {
        batch.valuesIterator.foreach(_.foreach(self ! IndexFile(_)))
      }

      batch.grouped(10).foreach(chunk => Future.sequence(chunk.map {
        case (outerClassFile, _) =>
          if (!outerClassFile.exists) {
            Future.successful(outerClassFile -> Nil)
          } else searchService.extractSymbolsFromClassOrJar(outerClassFile, batch).map(outerClassFile -> )
      }).onComplete {
        case Failure(t) =>
          log.error(t, s"failed to index batch of ${batch.size} files. $advice")
          retry()
        case Success(indexed) =>
          searchService.delete(indexed.flatMap(f => batch(f._1))(collection.breakOut)).onComplete {
            case Failure(t) =>
              log.error(t, s"failed to remove stale entries in ${batch.size} files. $advice")
              retry()
            case Success(_) => indexed.foreach {
              case (file, syms) =>
                val boost = searchService.isUserFile(file)
                val persisting = searchService.persist(syms, commitIndex = true, boost = boost)

                persisting.onComplete {
                  case Failure(t) =>
                    log.error(t, s"failed to persist entries in $file. $advice")
                    retry()
                  case Success(_) =>
                }
            }
          }

      })
  }

}
