package org.ensime.lsp

import monix.eval.Task

/**
 * An EitherT specifically of Task
 *
 * This should be provided by a functional library
 */
final case class EitherTask[L, A](value: Task[Either[L, A]]) {

  def flatMap[LL >: L, B](f: A => EitherTask[LL, B]): EitherTask[LL, B] =
    EitherTask[LL, B](value.flatMap {
      case Right(a) => f(a).value
      case Left(l)  => Task(Left(l))
    })

  def flatMapTask[B](f: A => Task[B]): EitherTask[L, B] =
    flatMap(a => EitherTask(f(a).map(Right(_))))

  def map[B](f: A => B): EitherTask[L, B] = EitherTask(value.map(_.map(f)))

  def leftMap[LL](f: L => LL): EitherTask[LL, A] =
    EitherTask(value.map(_.left.map(f)))

  def onErrorHandle(f: Throwable => L): EitherTask[L, A] =
    EitherTask(value.onErrorHandle(t => Left(f(t))))

  def raiseError(f: L => Throwable): Task[A] = value.flatMap {
    case Right(a) => Task(a)
    case Left(l)  => Task.raiseError(f(l))
  }

}

object EitherTask {

  def fromTask[L, A](task: Task[A]): EitherTask[L, A] =
    EitherTask(task.map(Right(_)))

  def fromEither[L, A](either: Either[L, A]): EitherTask[L, A] =
    EitherTask(Task(either))

  def fromLeft[L, A](l: L): EitherTask[L, A] = EitherTask.fromEither(Left(l))
}
