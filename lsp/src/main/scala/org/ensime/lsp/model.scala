package org.ensime.lsp

import akka.actor.ActorSystem
import monix.eval.{ MVar, Task }

/**
 * Represents the full mutable state of the ensime system
 *
 * @param actorSystem The ensime actor system.  This should be shut down on termination.
 * @param cache       A representation of the .ensimeCache directory.  This is used to store unzipped archives.
 * @param project     A wrapper for the [[org.ensime.core.Project]] actor
 */
final case class EnsimeState(actorSystem: ActorSystem,
                             cache: EnsimeCache,
                             project: EnsimeProjectWrapper)

final case class OptionalRef[A](state: MVar[Option[A]]) {

  def put(ref: A): Task[Unit] =
    for {
      _ <- state.take
      _ <- state.put(Some(ref))
    } yield ()

  def get: Task[Either[Uninitialized.type, A]] =
    state.read.map(_.toRight(Uninitialized))
}

object OptionalRef {
  def empty[A]: Task[OptionalRef[A]] =
    Task(MVar(Option.empty[A])).map(OptionalRef(_))
}

object Uninitialized {
  def message = "Ensime is uninitialized"
  def toIllegalArgumentException: IllegalArgumentException =
    new IllegalArgumentException("Ensime is uninitialized")
}

/**
  * Represents the scala and / or java signatures
  */
final case class Signature(scala: Option[String], java: Option[String])
