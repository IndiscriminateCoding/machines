package machines.machines

import cats.Eval
import cats.arrow.Category
import machines.machines.Plan._

trait Plan[K[_], F[_], O, A] { outer =>
  def apply[R](done: A => R, emit: (O, R) => R, await: Await[K, F, R], stop: R): R

  def map[B](f: A => B): Plan[K, F, O, B] = new Plan[K, F, O, B] {
    def apply[R](done: B => R, emit: (O, R) => R, await: Await[K, F, R], stop: R): R =
      outer(done compose f, emit, await, stop)
  }

  def flatMap[B](f: A => Plan[K, F, O, B]): Plan[K, F, O, B] = new Plan[K, F, O, B] {
    def apply[R](done: B => R, emit: (O, R) => R, await: Await[K, F, R], stop: R): R =
      outer(a => f(a)(done, emit, await, stop), emit, await, stop)
  }
}

object Plan {
  trait Await[K[_], F[_], R] {
    def await[Z](z: K[Z], e: Z => R, f: R): R

    def effect[Z](z: F[Z], e: Z => R): R

    def eval[Z](z: Eval[Z], e: Z => R): R
  }

  def stop[K[_], F[_], O, A]: Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](done: A => R, emit: (O, R) => R, await: Await[K, F, R], stop: R): R = stop
  }

  def pure[K[_], F[_], O, A](x: A): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](done: A => R, emit: (O, R) => R, await: Await[K, F, R], stop: R): R = done(x)
  }

  def emit[K[_], F[_], O](x: O): Plan[K, F, O, Unit] = new Plan[K, F, O, Unit] {
    def apply[R](done: Unit => R, emit: (O, R) => R, await: Await[K, F, R], stop: R): R =
      emit(x, done(()))
  }

  def awaits[K[_], F[_], O, A](f: K[A]): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](done: A => R, emit: (O, R) => R, await: Await[K, F, R], stop: R): R =
      await.await(f, done, stop)
  }

  def await[K[_, _], F[_], O, A](implicit K: Category[K]): Plan[K[A, ?], F, O, A] =
    awaits[K[A, ?], F, O, A](K.id)

  def fromEval[K[_], F[_], O, A](e: Eval[A]): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](done: A => R, emit: (O, R) => R, await: Await[K, F, R], stop: R): R =
      await.eval(e, done)
  }

  def lift[K[_], F[_], O, A](eff: F[A]): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](done: A => R, emit: (O, R) => R, await: Await[K, F, R], stop: R): R =
      await.effect(eff, done)
  }

  def shift[K[_], F[_], O]: Plan[K, F, O, Unit] = fromEval(Eval.Unit)
}
