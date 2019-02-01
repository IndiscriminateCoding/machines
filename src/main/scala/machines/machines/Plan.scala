package machines.machines

import cats.arrow.Category
import machines.machines.Plan._

trait Plan[K[_], F[_], O, A] { outer =>
  def apply[R](done: A => R, emit: (O, R) => R, await: Await[K, R], stop: R): R

  def map[B](f: A => B): Plan[K, F, O, B] = new Plan[K, F, O, B] {
    def apply[R](done: B => R, emit: (O, R) => R, await: Await[K, R], stop: R): R =
      outer(done compose f, emit, await, stop)
  }

  def flatMap[B](f: A => Plan[K, F, O, B]): Plan[K, F, O, B] = new Plan[K, F, O, B] {
    def apply[R](done: B => R, emit: (O, R) => R, await: Await[K, R], stop: R): R =
      outer(a => f(a)(done, emit, await, stop), emit, await, stop)
  }
}

object Plan {
  trait Await[K[_], R] {
    def apply[Z](z: K[Z], e: Z => R, f: R): R
  }

  def stop[K[_], F[_], O, A]: Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](done: A => R, emit: (O, R) => R, await: Await[K, R], stop: R): R = stop
  }

  def pure[K[_], F[_], O, A](x: A): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](done: A => R, emit: (O, R) => R, await: Await[K, R], stop: R): R = done(x)
  }

  def emit[K[_], F[_], O](x: O): Plan[K, F, O, Unit] = new Plan[K, F, O, Unit] {
    def apply[R](done: Unit => R, emit: (O, R) => R, await: Await[K, R], stop: R): R =
      emit(x, done(()))
  }

  def awaits[K[_], F[_], O, A](f: K[A]): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](done: A => R, emit: (O, R) => R, await: Await[K, R], stop: R): R =
      await(f, done, stop)
  }

  def await[K[_, _], F[_], O, A](implicit K: Category[K]): Plan[K[A, ?], F, O, A] =
    awaits[K[A, ?], F, O, A](K.id)
}
