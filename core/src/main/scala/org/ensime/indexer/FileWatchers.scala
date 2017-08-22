// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import java.util.UUID

import akka.actor.Actor
import akka.event.slf4j.SLF4JLogging
import scala.util.Properties._

import org.ensime.api._
import org.ensime.config.richconfig._
import org.ensime.util.file._
import org.ensime.vfs._

trait FileChangeListener {
  def fileAdded(f: EnsimeFile): Unit
  def fileRemoved(f: EnsimeFile): Unit
  def fileChanged(f: EnsimeFile): Unit
  def baseReCreated(@deprecated("local", "") f: EnsimeFile): Unit = {}
  def baseRemoved(@deprecated("local", "") f: EnsimeFile): Unit = {}
  def baseRegistered(): Unit = {}
}

trait Watcher {
  def shutdown(): Unit
}

/**
 * Watches the user's target output directories for classfiles that
 * need to be indexed or updated (i.e. picks up changes when the
 * compiler produces any output). Can also support jars instead of
 * target directories.
 *
 * If we were Java 7+ we'd be using
 * http://docs.oracle.com/javase/7/docs/api/java/nio/file/WatchService.html
 */
class ClassfileWatcher(
    listeners: Seq[FileChangeListener]
)(
    implicit
    config: EnsimeConfig,
    serverConfig: EnsimeServerConfig
) extends Actor with SLF4JLogging {

  private val impls =
    if (serverConfig.disableClassMonitoring) Nil
    else {
      val jarJava7WatcherBuilder = new JarJava7WatcherBuilder()
      val classJava7WatcherBuilder = new ClassJava7WatcherBuilder()
      config.targets.map { target =>
        if (target.isJar) {
          if (log.isTraceEnabled())
            log.trace(s"creating a Java 7 jar watcher for ${target}")
          jarJava7WatcherBuilder.build(target, listeners)
        } else {
          if (log.isTraceEnabled())
            log.trace(s"creating a Java 7 class watcher for ${target}")
          classJava7WatcherBuilder.build(target, listeners)
        }
      }
    }

  override def receive: Receive = {
    case _ =>
  }

  override def postStop(): Unit = {
    impls.foreach(_.shutdown())
  }
}

trait Java7WatcherBuilder extends SLF4JLogging {
  import org.ensime.filewatcher.WatcherListener
  val serviceBuilder = new Java7WatchServiceBuilder()
  def build(
    watched: File,
    listeners: Seq[FileChangeListener]
  ): Watcher = {
    val watcherId = UUID.randomUUID()
    serviceBuilder.build(watcherId, watched,
      listeners.map { l => toWatcherListener(l, watched, watcherId) })
  }
  def toWatcherListener(
    l: FileChangeListener,
    baseFile: File,
    uuid: UUID,
  ): WatcherListener
}

class JarJava7WatcherBuilder() extends Java7WatcherBuilder {
  import org.ensime.filewatcher.WatcherListener
  override def toWatcherListener(
    l: FileChangeListener,
    baseFile: File,
    uuid: UUID,
  ) = {
    new WatcherListener() {
      override val base = baseFile
      override val recursive = false
      override val extensions = JarSelector.include
      override val watcherId = uuid
      //TODO: Can we meet the jars here, i.e. shall we use EnsimeFile?
      override def fileCreated(f: File) =
        l.fileAdded(RawFile(f.toPath))
      override def fileDeleted(f: File) = {}
      override def fileModified(f: File) =
        l.fileChanged(RawFile(f.toPath))
      override def baseRegistered(): Unit =
        l.baseRegistered()
      override def baseRemoved(): Unit =
        l.fileRemoved(RawFile(baseFile.toPath))
      override def missingBaseRegistered(): Unit =
        l.fileAdded(RawFile(baseFile.toPath))
      override def baseSubdirRegistered(f: File): Unit = {}
      override def proxyRegistered(f: File): Unit = {}
      override def existingFile(f: File): Unit = {}
    }
  }

}

private class ClassJava7WatcherBuilder() extends Java7WatcherBuilder {
  import org.ensime.filewatcher.WatcherListener
  override def toWatcherListener(
    l: FileChangeListener,
    baseFile: File,
    uuid: UUID,
  ) = {
    new WatcherListener() {
      override val base = baseFile
      override val recursive = true
      override val extensions = ClassfileSelector.include
      override val watcherId = uuid
      @volatile private var notifyExisting = false;

      override def fileCreated(f: File) =
        l.fileAdded(RawFile(f.toPath))
      override def fileDeleted(f: File) =
        l.fileRemoved(RawFile(f.toPath))
      override def fileModified(f: File) =
        l.fileChanged(RawFile(f.toPath))
      override def baseRegistered(): Unit = {
        notifyExisting = true
        l.baseRegistered()
      }
      override def baseRemoved(): Unit =
        l.baseRemoved(RawFile(baseFile.toPath))
      override def missingBaseRegistered(): Unit =
        l.baseReCreated(RawFile(baseFile.toPath))
      override def baseSubdirRegistered(f: File): Unit = {}
      override def proxyRegistered(f: File): Unit =
        notifyExisting = true
      override def existingFile(f: File): Unit =
        if (notifyExisting)
          l.fileAdded(RawFile(f.toPath))
    }
  }
}

class Java7WatchServiceBuilder extends SLF4JLogging {
  import org.ensime.filewatcher.{ FileWatchService, WatcherListener }

  val fileWatchService: FileWatchService = new FileWatchService
  def build(
    watcherId: UUID,
    base: File,
    listeners: Seq[WatcherListener]
  ) = {
    if (log.isTraceEnabled())
      log.trace("watching {}", base)

    trait EnsimeWatcher extends Watcher {
      val w = fileWatchService.spawnWatcher(watcherId, base, listeners.toSet)
      override def shutdown(): Unit = {
        if (log.isDebugEnabled())
          log.debug("shutdown watcher {}", w.watcherId)
        w.shutdown()
      }
    }
    val ensimeWatcher = new EnsimeWatcher {}
    ensimeWatcher
  }
}
