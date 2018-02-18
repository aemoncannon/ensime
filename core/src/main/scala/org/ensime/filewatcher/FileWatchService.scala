// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.filewatcher

import java.io._
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.{FileSystems, Path, WatchKey, WatchService}
import java.util.UUID

import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.collection.immutable.Set
import scala.language.implicitConversions
import scala.util.control.NonFatal
import scala.util.{Failure, Properties, Success, Try}

abstract class Watcher(val watcherId: UUID,
                       val file: File,
                       val listeners: Set[WatcherListener]) {
  val fileWatchService: FileWatchService
  def watch(): Unit =
    fileWatchService.watch(file, listeners, false)

  def shutdown(): Unit = {
    WatchKeyManager.removeObservers(watcherId)
    fileWatchService.monitorThread.foreach { thread =>
      thread.interrupt()
    }
  }
}


// tested in FileWatcherSpec
class FileWatchService { self =>
  private val log = LoggerFactory.getLogger(getClass)

  /**
   * The low priority thread used for checking the files being monitored.
   */
  @volatile private[filewatcher] var monitorThread: Option[Thread] = None

  /**
   * A flag used to determine if the monitor thread should be running.
   */
  @volatile private var shouldRun: Boolean = true

  /**
   * Construct a new Java7 WatchService
   */
  var watchService: WatchService = null

  implicit def keyToFile(k: WatchKey): File =
    k.watchable().asInstanceOf[Path].toFile
  implicit def keyToCanonicalPath(k: WatchKey): String =
    k.watchable().asInstanceOf[Path].toFile.getCanonicalPath()

  private def init(): Unit = {
    log.debug("init watcher")
    watchService = Try {
      FileSystems.getDefault().newWatchService()
    } match {
      case Success(w) => w
      case Failure(e) =>
        throw new Exception("failed to create WatchService {}", e)
    }
    start()
  }

  /**
   * Start a background monitoring thread
   */
  private def start() = {
    log.debug("start a background monitoring thread")
    monitorThread match {
      case Some(t) => log.warn(s"monitoring thread is already started")
      case None => {
        val thread = new Thread(
          new Runnable {
            override def run(): Unit =
              try monitor()
              catch {
                case i: InterruptedException => // silently ignore
                case NonFatal(e) =>
                  log.warn(s"caught an exception while monitoring", e)
              }
          }
        )
        thread.setName("FileWatchService-monitor")
        thread.setDaemon(true)
        thread.start()
        monitorThread = Some(thread)
      }
    }
  }

  def watch(file: File,
            listeners: Set[WatcherListener],
            wasMissing: Boolean,
            retry: Int = 2): Unit =
    try {
      if (file.isDirectory) {
        registerDir(file, listeners, wasMissing, retry)
      } else if (file.isFile) {
        val fileBase = new File(file.getParent)
        registerDir(fileBase, listeners, wasMissing, retry)
      } else {
        if (file.getParentFile.exists) {
          registerDir(file.getParentFile, listeners, wasMissing, retry)
        } else {
          watch(file.getParentFile, listeners, wasMissing, retry)
        }
      }
    } catch {
      case e: Throwable =>
        log.error(s"failed to watch ${file}")
    }

  def notifyExisting(dir: File, listeners: Set[WatcherListener]) =
    for {
      f <- dir.listFiles
      if f.isFile
      l <- listeners
      if l.isWatched(f)
    } { l.existingFile(f) }

  def watchExistingSubdirs(dir: File, listeners: Set[WatcherListener]) =
    if (listeners.exists(_.recursive))
      for {
        d <- dir.listFiles
        if d.isDirectory
        l <- listeners
      } { watch(d, listeners, false) }

  def registerDir(dir: File,
                  listeners: Set[WatcherListener],
                  wasMissing: Boolean,
                  retry: Int = 2): Unit = {
    if (wasMissing && listeners.exists { l =>
          l.base == dir
        }) {
      if (log.isTraceEnabled)
        log.trace(s"delay ${dir} base registration")
      Thread.sleep(100)
    }

    val observers = listeners flatMap {
        maybeBuildWatchKeyObserver(dir, _)
      }

    if (log.isTraceEnabled)
      log.trace(s"register ${dir} with WatchService")

    if (observers.nonEmpty) {
      val key: WatchKey = try {
        dir.toPath.register(
          watchService,
          ENTRY_CREATE,
          ENTRY_MODIFY,
          ENTRY_DELETE
        )
      } catch {
        case e: Throwable => {
          if (retry < 0) {
            log.warn("can not register. retrying..." + dir + " " + e)
            Thread.sleep(50)
            watch(dir, listeners, wasMissing, retry - 1)
          }
          throw new Exception(e)
        }
      }

      notifyExisting(dir, listeners)
      if (observers.exists {
            case _: BaseObserver       => true
            case _: BaseSubdirObserver => true
            case _: BaseFileObserver   => true
            case _                     => false
          })
        watchExistingSubdirs(dir, listeners)

      observers foreach {
        case o: BaseObserver =>
          if (wasMissing)
            o.watcherListener.missingBaseRegistered()
          else
            o.watcherListener.baseRegistered()
        case o: BaseFileObserver =>
          if (wasMissing)
            o.watcherListener.missingBaseRegistered()
          else
            o.watcherListener.baseRegistered()
        case o: BaseSubdirObserver =>
          o.watcherListener.baseSubdirRegistered(dir)

        case o: ProxyObserver => o.watcherListener.proxyRegistered(dir)
      }

      observers foreach (WatchKeyManager.addObserver(key, _))
      if (WatchKeyManager.hasProxy(key))
        dir.listFiles
          .filter(f => (f.isDirectory || f.isFile))
          .foreach(WatchKeyManager.maybeAdvanceProxy(key, _)(this))

    } else
      log.warn("No listeners for {}. Skip registration.")
  }

  /**
   *  Wait for Java7 WatchService event and notify the listeners.
   */
  private def monitor() = {
    log.debug("start monitoring WatchService events")
    while (continueMonitoring) {
      Try { watchService.take() } match {
        case Success(key) => {
          if (WatchKeyManager.contains(key)) {
            processEvents(key)
            val isWindows = Properties.osName.startsWith("Windows")
            // can not recover reliably from deleted base without delay
            if (isWindows) Thread.sleep(1000)
            else Thread.sleep(20)
            if (!key.reset) {
              if (log.isTraceEnabled)
                log.trace("may be recover from deletion {}", keyToFile(key))
              maybeRecoverFromDeletion(key)
            }
          } else if (log.isTraceEnabled)
            log.trace(s"key {} is not managed by watcher yet", keyToFile(key))
        }
        case Failure(e) => {
          log.error("unexpected WatchService take error. {}", e)
          shouldRun = false
        }
      }
    }
    closeWatchService()

    def processEvents(key: WatchKey) =
      for (event <- key.pollEvents.asScala) {
        val kind = event.kind
        val file = key.watchable
          .asInstanceOf[Path]
          .resolve(event.context.asInstanceOf[Path])
          .toFile

        if (kind == ENTRY_CREATE
            && file.isDirectory
            && WatchKeyManager.hasRecursive(key))
          watch(file, WatchKeyManager.recListeners(key), false)

        if (kind == ENTRY_CREATE)
          WatchKeyManager.maybeAdvanceProxy(key, file)(this)

        val ls = WatchKeyManager.nonProxyListeners(key)

        if (kind == ENTRY_CREATE)
          ls filter { _.isWatched(file) } foreach (_.fileCreated(file))

        if (kind == ENTRY_MODIFY)
          ls filter { _.isWatched(file) } foreach (_.fileModified(file))

        if (kind == ENTRY_DELETE) {
          ls filter { _.isWatched(file) } foreach (_.fileDeleted(file))
          for {
            o <- WatchKeyManager.baseFileObservers(key)
            if o.watcherListener.isWatched(file)
          } {
            WatchKeyManager.removeObserver(key, o)
            o.watcherListener.baseRemoved()
            watch(file, Set(o.watcherListener), true)
          }
        }
        if (kind == OVERFLOW)
          log.warn(s"overflow event for ${file}")
      }

    def maybeRecoverFromDeletion(key: WatchKey, retry: Int = 0): Unit =
      if (WatchKeyManager.hasBase(key)
          || WatchKeyManager.hasBaseFile(key)
          || WatchKeyManager.hasProxy(key)) {
        if (log.isTraceEnabled)
          log.trace("recover from deletion {}", keyToFile(key))

        if (!key.mkdirs && !key.exists) {
          if (retry <= 3) {
            Thread.sleep(20)
            log.error("retry re-create {} with parents", keyToFile(key))
            maybeRecoverFromDeletion(key, retry + 1)
          }
          log.error("Unable to re-create {} with parents", keyToFile(key))
        } else {
          val listeners         = WatchKeyManager.listeners(key)
          val baseFileListeners = WatchKeyManager.baseFileListeners(key)
          listeners foreach (_.baseRemoved())
          baseFileListeners foreach (o => o.fileDeleted(o.base))
          WatchKeyManager.removeKey(key)
          watch(key, listeners, true)
        }
      } else if (WatchKeyManager.hasSubDir(key)) {
        WatchKeyManager.keyFromFile(key.getParentFile) match {
          case Some(p) => maybeRecoverFromDeletion(p)
          case None    => log.warn(s"can not find a parent key")
        }
      }

    def continueMonitoring() =
      (monitorThread match {
        case Some(t) =>
          if (t.isInterrupted) {
            log.info("monitoring thread was interrupted")
            false
          } else true
        case None => {
          log.info("monitoring should run in a background thread")
          false
        }
      }) && shouldRun
  }

  def closeWatchService() =
    try {
      log.info("close  WatchService")
      shouldRun = false
      watchService.close();
    } catch {
      case e: Throwable =>
        log.error("failed to close WatchService {}", e);
    }

  def spawnWatcher(file: File, listeners: Set[WatcherListener]): Watcher =
    spawnWatcher(UUID.randomUUID(), file, listeners)

  def spawnWatcher(uuid: UUID, file: File, listeners: Set[WatcherListener]) = {
    if (log.isTraceEnabled)
      log.trace(s"spawn ${uuid} watcher for ${file} base")
    val w = new Watcher(uuid, file, listeners) {
      val fileWatchService = self;
    }
    w.watch()
    w
  }

  def maybeBuildWatchKeyObserver(
    f: File,
    l: WatcherListener
  ): Option[WatchKeyObserver] = {
    if (!f.isDirectory) {
      log.warn(
        "building a WatchKeyObserver for a non-existent {} doesn't make sense.",
        f
      )
      return None
    }
    if (l.base == f)
      Some(new BaseObserver(l))
    else if (l.base.isFile && l.base.getParentFile == f)
      Some(new BaseFileObserver(l))
    else if (l.recursive && f.getAbsolutePath.startsWith(
               l.base.getAbsolutePath
             ))
      Some(new BaseSubdirObserver(l))
    else if (l.base.getAbsolutePath.startsWith(f.getAbsolutePath))
      Some(new ProxyObserver(l))
    else {
      log.warn(
        s"don't know what observer to create dir: ${f} for ${l.base.getAbsolutePath} base"
      )
      None
    }
  }

  init()

}
