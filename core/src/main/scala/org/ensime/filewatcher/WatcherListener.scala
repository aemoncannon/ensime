package org.ensime.filewatcher

import java.io.File
import java.util.UUID

trait WatcherListener {
  val base: File
  val recursive: Boolean
  val extensions: scala.collection.Set[String]
  val watcherId: UUID

  def fileCreated(@deprecated("local", "") f: File): Unit  = {}
  def fileDeleted(@deprecated("local", "") f: File): Unit  = {}
  def fileModified(@deprecated("local", "") f: File): Unit = {}

  def baseRegistered(): Unit                                       = {}
  def baseRemoved(): Unit                                          = {}
  def baseSubdirRemoved(@deprecated("local", "") f: File): Unit    = {}
  def missingBaseRegistered(): Unit                                = {}
  def baseSubdirRegistered(@deprecated("local", "") f: File): Unit = {}
  def proxyRegistered(@deprecated("local", "") f: File): Unit      = {}

  def existingFile(@deprecated("local", "") f: File): Unit = {}

  def isWatched(f: File) =
    (extensions.exists(e => {
      f.getName.endsWith(e)
    })) && f.getPath.startsWith(base.getPath)

  def isBaseAncestor(f: File) =
    base.getAbsolutePath.startsWith(f.getAbsolutePath)
}
