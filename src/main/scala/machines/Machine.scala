package machines

import cats.Monad
import machines.Machine._

import scala.collection.generic.CanBuild

sealed trait Machine[K[_], F[_], O] {
  final def map[A](f: O => A): Machine[K, F, A] = this match {
    case _: Stop[K, F, O] => Stop()
    case e: Emit[K, F, O] => Emit(f(e.h), e.t.map(f))
    case a: Await[K, F, O] => Await(a.await, a(_: a.Z).map(f), a.stop.map(f))
    case e: Effect[K, F, O] => Effect(e.effect, e(_: e.Z).map(f))
    case s: Shift[K, F, O] => Shift(s().map(f))
  }

  final def run_(implicit F: Monad[F]): F[Unit] = this match {
    case _: Stop[K, F, O] => F.unit
    case e: Emit[K, F, O] => e.t.run_
    case a: Await[K, F, O] => a.stop.run_
    case e: Effect[K, F, O] => F.flatMap(e.effect)(z => e(z).run_)
    case s: Shift[K, F, O] => s().run_
  }

  final def foldLeft[A](acc: A)(f: (A, O) => A)(implicit F: Monad[F]): F[A] = this match {
    case _: Stop[K, F, O] => F.pure(acc)
    case e: Emit[K, F, O] => e.t.foldLeft(f(acc, e.h))(f)
    case a: Await[K, F, O] => a.stop.foldLeft(acc)(f)
    case e: Effect[K, F, O] => F.flatMap(e.effect)(z => e(z).foldLeft(acc)(f))
    case s: Shift[K, F, O] => s().foldLeft(acc)(f)
  }

  final def foldLeftM[A](acc: A)(f: (A, O) => F[A])(implicit F: Monad[F]): F[A] = this match {
    case _: Stop[K, F, O] => F.pure(acc)
    case e: Emit[K, F, O] => F.flatMap(f(acc, e.h))(a => e.t.foldLeftM(a)(f))
    case a: Await[K, F, O] => a.stop.foldLeftM(acc)(f)
    case e: Effect[K, F, O] => F.flatMap(e.effect)(z => e(z).foldLeftM(acc)(f))
    case s: Shift[K, F, O] => s().foldLeftM(acc)(f)
  }

  final def to[T[_]](implicit C: CanBuild[O, T[O]], F: Monad[F]): F[T[O]] =
    F.map(foldLeft[List[O]](Nil)((a, o) => o :: a)) { l =>
      val b = C()
      l.reverseIterator.foreach(b += _)
      b.result()
    }

  final def toList(implicit F: Monad[F]): F[List[O]] = to[List]

  final def toVector(implicit F: Monad[F]): F[Vector[O]] = to[Vector]
}

object Machine {
  final class Stop[K[_], F[_], O] extends Machine[K, F, O]
  object Stop {
    @inline def apply[K[_], F[_], O](): Stop[K, F, O] = new Stop
  }

  final class Emit[K[_], F[_], O](val h: O, val t: Machine[K, F, O]) extends Machine[K, F, O]
  object Emit {
    @inline def apply[K[_], F[_], O](h: O, t: Machine[K, F, O]): Emit[K, F, O] = new Emit(h, t)
  }

  sealed trait Await[K[_], F[_], O] extends Machine[K, F, O] {
    type Z

    def await: K[Z]
    def apply(z: Z): Machine[K, F, O]
    def stop: Machine[K, F, O]
  }
  object Await {
    @inline def apply[E, K[_], F[_], O](
      z: K[E],
      e: E => Machine[K, F, O],
      f: Machine[K, F, O]
    ): Await[K, F, O] = new Await[K, F, O] {
      type Z = E

      val await: K[Z] = z
      val stop: Machine[K, F, O] = f

      def apply(z: Z): Machine[K, F, O] = e(z)
    }
  }

  sealed trait Effect[K[_], F[_], O] extends Machine[K, F, O] {
    type Z

    def effect: F[Z]
    def apply(z: Z): Machine[K, F, O]
  }
  object Effect {
    @inline def apply[E, K[_], F[_], O](z: F[E], e: E => Machine[K, F, O]): Effect[K, F, O] =
      new Effect[K, F, O] {
        type Z = E

        val effect: F[Z] = z

        def apply(z: Z): Machine[K, F, O] = e(z)
      }
  }

  sealed trait Shift[K[_], F[_], O] extends Machine[K, F, O] {
    def apply(): Machine[K, F, O]
  }
  object Shift {
    @inline def apply[K[_], F[_], O](@inline m: => Machine[K, F, O]): Shift[K, F, O] =
      new Shift[K, F, O] {
        def apply(): Machine[K, F, O] = m
      }
  }
}
