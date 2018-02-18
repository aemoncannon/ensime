package org.ensime.filewatcher

import java.io.File
import java.nio.file.{Path, WatchKey}
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.collection.concurrent.Map
import scala.collection.immutable.Set
import scala.collection.JavaConverters._
import scala.language.implicitConversions

case class BaseObserver(val watcherListener: WatcherListener)
  extends WatchKeyObserver {
  override lazy val recursive = watcherListener.recursive
  override val observerType   = "BaseObserver"
}
case class BaseFileObserver(val watcherListener: WatcherListener)
  extends WatchKeyObserver {
  val treatExistingAsNew    = true
  val recursive             = false
  override val observerType = "BaseFileObserver"
}
case class ProxyObserver(val watcherListener: WatcherListener)
  extends WatchKeyObserver {
  val recursive             = false
  override val observerType = "ProxyObserver"
}
case class BaseSubdirObserver(val watcherListener: WatcherListener)
  extends WatchKeyObserver {
  override lazy val recursive = watcherListener.recursive
  override val observerType   = "BaseSubdirObserver"
}

trait WatchKeyObserver {
  val watcherListener: WatcherListener
  val recursive: Boolean
  val observerType: String
}

object WatchKeyManager {
  private val log = LoggerFactory.getLogger(getClass)

  val keymap: Map[WatchKey, Set[WatchKeyObserver]] =
    new ConcurrentHashMap[WatchKey, Set[WatchKeyObserver]]().asScala

  def contains(key: WatchKey) =
    keymap.contains(key)

  implicit def keyToFile(k: WatchKey): File =
    k.watchable().asInstanceOf[Path].toFile
  implicit def keyToCanonicalPath(k: WatchKey): String =
    k.watchable().asInstanceOf[Path].toFile.getCanonicalPath()

  @tailrec
  def addObserver(key: WatchKey, o: WatchKeyObserver): Unit = {
    val l            = Set[WatchKeyObserver]()
    val oldListeners = keymap.putIfAbsent(key, l).getOrElse(l)
    val newListeners = oldListeners + o
    val status       = keymap.replace(key, oldListeners, newListeners)
    if (!status) {
      log.warn(s"retry adding ${o.observerType} to ${keyToFile(key)}")
      addObserver(key, o)
    }
  }

  @tailrec
  def removeObserver(key: WatchKey,
                     o: WatchKeyObserver,
                     retry: Int = 2): Unit =
    keymap.get(key) match {
      case Some(oldObservers) => {
        val newObservers = oldObservers - o
        if (newObservers.isEmpty) {
          keymap.remove(key)
          key.cancel()
        } else if (!keymap.replace(key, oldObservers, newObservers))
          if (retry > 0)
            removeObserver(key, o)
          else
            log.warn("unable to remove an observer from {}", keyToFile(key))
      }
      case None => log.warn(s"watcher doesn't monitor ${keyToFile(key)}")
    }

  def maybeAdvanceProxy(key: WatchKey, createdFile: File)(fws : FileWatchService) =
    proxies(key) foreach (
      o =>
        if (o.watcherListener.isBaseAncestor(createdFile))
          if (createdFile.isDirectory || createdFile.isFile) {
            removeObserver(key, o)
            fws.watch(createdFile, Set(o.watcherListener), true)
          } else
            log.warn("unable to advance a proxy {}", o)
      )

  def removeObservers(id: UUID) =
    keymap.keys foreach (
      key => {
        val observers = keymap.get(key).getOrElse { Set() }
        val unneeded  = observers filter { _.watcherListener.watcherId == id }
        val retained  = observers filter { _.watcherListener.watcherId != id }

        if (observers.size == 0 || unneeded.size == observers.size) {
          key.cancel() // can hang, https://bugs.openjdk.java.net/browse/JDK-8029516
          keymap.remove(key)
        } else if (observers.size != retained.size)
          if (!keymap.replace(key, observers, retained))
            log.error(
              s"failed to remove ${unneeded.size} listeners from  ${keyToFile(key)}"
            )
      }
      )

  def baseFileObservers(key: WatchKey) =
    keymap getOrElse (key, Set()) filter {
      case _: BaseFileObserver => true
      case _                   => false
    }

  def recListeners(key: WatchKey) =
    listeners(key) filter { _.recursive }

  def baseListeners(key: WatchKey) =
    keymap getOrElse (key, Set()) filter {
      case _: BaseObserver => true
      case _               => false
    } map { _.watcherListener }

  def baseFileListeners(key: WatchKey) =
    keymap getOrElse (key, Set()) filter {
      case _: BaseFileObserver => true
      case _                   => false
    } map { _.watcherListener }

  def proxyListeners(key: WatchKey) =
    keymap getOrElse (key, Set()) filter {
      case _: ProxyObserver => true
      case _                => false
    } map { _.watcherListener }

  def nonProxyListeners(key: WatchKey) =
    keymap getOrElse (key, Set()) filter {
      case _: ProxyObserver => false
      case _                => true
    } map { _.watcherListener }

  def proxies(key: WatchKey) =
    keymap getOrElse (key, Set()) filter {
      case _: ProxyObserver => true
      case _                => false
    }

  def listeners(key: WatchKey) =
    keymap getOrElse (key, Set()) map { _.watcherListener }

  def removeKey(key: WatchKey): Unit = {
    key.cancel()
    keymap.remove(key)
  }

  def hasRecursive(key: WatchKey) =
    keymap.get(key) match {
      case Some(os) => os.exists { _.recursive }
      case None     => false
    }

  def hasBase(key: WatchKey) =
    keymap.get(key) match {
      case Some(os) =>
        os.exists {
          case _: BaseObserver => true
          case _               => false
        }
      case None => false
    }

  def hasSubDir(key: WatchKey) =
    keymap.get(key) match {
      case Some(os) =>
        os.exists {
          case _: BaseSubdirObserver => true
          case _                     => false
        }
      case None => false
    }

  def hasBaseFile(key: WatchKey) =
    keymap.get(key) match {
      case Some(os) =>
        os.exists {
          case _: BaseFileObserver => true
          case _                   => false
        }
      case None => false
    }

  def hasProxy(key: WatchKey) =
    keymap.get(key) match {
      case Some(os) =>
        os.exists {
          case _: ProxyObserver => true
          case _                => false
        }
      case None => false
    }

  def hasBaseSubdir(key: WatchKey) =
    keymap.get(key) match {
      case Some(os) =>
        os.exists {
          case _: BaseSubdirObserver => true
          case _                     => false
        }
      case None => false
    }

  def totalKeyNum() =
    keymap.keys.foldLeft(0) { (a, _) =>
      a + 1
    }

  def keyFromFile(f: File): Option[WatchKey] =
    keymap.keys.find { k =>
      keyToFile(k).getAbsolutePath == f.getAbsolutePath
    }
}