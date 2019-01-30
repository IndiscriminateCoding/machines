package machines.machines

import cats.arrow.Category
import machines.machines.Plan._

trait Plan[F[_], O, A] { outer =>
  def apply[R](done: A => R, emit: (O, R) => R, await: Await[F, R], stop: R): R

  def map[B](f: A => B): Plan[F, O, B] = new Plan[F, O, B] {
    def apply[R](done: B => R, emit: (O, R) => R, await: Await[F, R], stop: R): R =
      outer(done compose f, emit, await, stop)
  }

  def flatMap[B](f: A => Plan[F, O, B]): Plan[F, O, B] = new Plan[F, O, B] {
    def apply[R](done: B => R, emit: (O, R) => R, await: Await[F, R], stop: R): R =
      outer(a => f(a)(done, emit, await, stop), emit, await, stop)
  }
}

object Plan {
  trait Await[F[_], R] {
    def apply[Z](z: F[Z], e: Z => R, f: R): R
  }

  def stop[F[_], O, A]: Plan[F, O, A] = new Plan[F, O, A] {
    def apply[R](done: A => R, emit: (O, R) => R, await: Await[F, R], stop: R): R = stop
  }

  def pure[F[_], O, A](x: A): Plan[F, O, A] = new Plan[F, O, A] {
    def apply[R](done: A => R, emit: (O, R) => R, await: Await[F, R], stop: R): R = done(x)
  }

  def emit[F[_], O](x: O): Plan[F, O, Unit] = new Plan[F, O, Unit] {
    def apply[R](done: Unit => R, emit: (O, R) => R, await: Await[F, R], stop: R): R =
      emit(x, done(()))
  }

  def awaits[F[_], O, A](f: F[A]): Plan[F, O, A] = new Plan[F, O, A] {
    def apply[R](done: A => R, emit: (O, R) => R, await: Await[F, R], stop: R): R =
      await.apply(f, done, stop)
  }

  def await[K[_, _], O, A](implicit K: Category[K]): Plan[K[A, ?], O, A] =
    awaits[K[A, ?], O, A](K.id)
}
