// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import java.nio.file.{ FileSystems, Files, Path }

import akka.actor._
import akka.event.slf4j.SLF4JLogging

import org.ensime.api._
import org.ensime.util.Debouncer
import org.ensime.util.ensimefile._
import org.ensime.util.list._
import org.ensime.util.map._

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.duration._

// mutable: lookup of user's source files are atomically updated
class SourceResolver(config: EnsimeConfig)(implicit actorSystem: ActorSystem)
    extends FileChangeListener with SLF4JLogging {

  def fileAdded(f: RawFile) = if (relevant(f)) debouncedUpdate.call()
  def fileRemoved(f: RawFile) = debouncedUpdate.call()
  def fileChanged(f: RawFile) = {}

  def relevant(f: EnsimeFile): Boolean = Files.isRegularFile(f.path) && {
    (f.isScala || f.isJava) && !f.path.toString.contains(".ensime_cache")
  }

  def resolve(clazz: PackageName, source: RawSource): Option[EnsimeFile] = {
    @tailrec
    def loop(clazzes: List[PackageName]): Option[EnsimeFile] = {
      clazzes match {
        case Nil => None
        case h :: t => resolveClazz(h, source) match {
          case None => loop(t)
          case s @ Some(_) => s
        }
      }
    }

    val size = clazz.path.size
    val combinations = clazz.path.tails.flatMap(_.inits).filterNot(_.isEmpty).toList
    // Quite offen people put stuff into the root package,
    // so we add empty package after parent packages, just
    // before we try other possible packages
    val combinationsWithEmpty =
      (combinations.take(size) ::: List.empty[String] :: combinations.drop(size))
        .map(PackageName.apply)
    loop(combinationsWithEmpty)
  }

  // we only support the case where RawSource has a Some(filename)
  private def resolveClazz(clazz: PackageName, source: RawSource): Option[EnsimeFile] =
    source.filename match {
      case None => None
      case Some(filename) => all.get(clazz).flatMap {
        // TODO: filename could be path?
        _.find(_.path.getFileName.toString == filename)
      }
    }

  def update(): Unit = {
    log.debug("updating sources")
    all = recalculate
  }

  private def scanDir(f: RawFile) = {
    Files.find(
      f.path,
      Integer.MAX_VALUE,
      (path, attrs) => {
        val name = path.toString
        name.endsWith(".java") || name.endsWith(".scala")
      }
    ).iterator.asScala.toList
      .map(p => EnsimeFile(p.toFile))
  }

  //TODO: Use vector?
  private def scanArchive(f: RawFile): Vector[(EnsimeFile, PackageName)] = {
    val fs = FileSystems.newFileSystem(f.path, null)
    val path = fs.getPath("/")
    val all = Files.find(
      path,
      Integer.MAX_VALUE,
      (path, attrs) => {
        val name = path.toString
        name.endsWith(".java") || name.endsWith(".scala")
      }
    ).iterator
      .asScala
      .map(p => (ArchiveFile(f.path, p.toString), inferArchive(p)))
      .toVector

    fs.close()
    all
  }

  private val depSources: Map[PackageName, Set[EnsimeFile]] = {
    val srcJars = config.javaSources.toSet ++ config.projects.flatMap(_.librarySources) ++ {
      for {
        project <- config.projects
        srcArchive <- project.librarySources
      } yield srcArchive
    }
    for {
      srcJarFile <- srcJars.toList
      (archFile, inferred) <- scanArchive(srcJarFile)
      // continue to hold a reference to source jars
      // so that we can access their contents elsewhere.
      // this does mean we have a file handler, sorry.
      //_ = vfs.nuke(srcJar)
    } yield (inferred, archFile)
  }.toMultiMapSet

  private def userSources = {
    for {
      project <- config.projects
      root <- project.sources
      file <- scanDir(root)
    } yield (infer(root, file), file)
  }.toMultiMapSet

  private def recalculate = depSources merge userSources

  private var all = recalculate

  val debouncedUpdate = {
    import actorSystem.dispatcher
    Debouncer("SourceResolver", actorSystem.scheduler, delay = 5.seconds, maxDelay = 1.hour) { () =>
      this.update()
    }
  }

  private def infer(base: EnsimeFile, file: EnsimeFile): PackageName = {
    // relativize feels the wrong way round, but this is correct
    val relative = base.path.relativize(file.path)
    PackageName(relative.iterator.asScala.toList.init.map(_.toString))
  }

  private def inferArchive(p: Path): PackageName = {
    PackageName(p.iterator.asScala.toList.init.map(_.toString))
  }

}
