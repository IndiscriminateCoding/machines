package machines

import machines.Machine._

sealed trait Machine[+F[_], +K[_], +O] {
  final def map[A](f: O => A): Machine[F, K, A] = this match {
    case Stop => Stop
    case Emit(h, t) => Emit(f(h), t.map(f)) // TODO: stack safety
    case a: Await[F, K, O] => Await(a.await, a(_: a.Z).map(f), a.stop.map(f))
    case e: Effect[F, K, O] => Effect(e.effect, e(_: e.Z).map(f))
    case s: Shift[F, K, O] => Shift(s().map(f))
  }
}

object Machine {
  object Stop extends Machine[Nothing, Nothing, Nothing]

  final class Emit[+F[_], +K[_], +O](val h: O, val t: Machine[F, K, O]) extends Machine[F, K, O]
  object Emit {
    def apply[F[_], K[_], O](h: O, t: Machine[F, K, O]): Emit[F, K, O] = new Emit(h, t)

    def unapply[F[_], K[_], O](m: Machine[F, K, O]): Option[(O, Machine[F, K, O])] =
      m match {
        case emit: Emit[F, K, O] => Some(emit.h -> emit.t)
        case _ => None
      }
  }

  sealed trait Await[+F[_], +K[_], +O] extends Machine[F, K, O] {
    type Z

    def await: K[Z]

    def apply(z: Z): Machine[F, K, O]

    def stop: Machine[F, K, O]
  }
  object Await {
    def apply[E, F[_], K[_], O](
      z: K[E],
      e: E => Machine[F, K, O],
      f: Machine[F, K, O]
    ): Await[F, K, O] = new Await[F, K, O] {
      type Z = E

      val await: K[Z] = z
      val stop: Machine[F, K, O] = f

      def apply(z: Z): Machine[F, K, O] = e(z)
    }
  }

  sealed trait Effect[+F[_], +K[_], +O] extends Machine[F, K, O] {
    type Z

    def effect: F[Z]

    def apply(z: Z): Machine[F, K, O]
  }
  object Effect {
    def apply[E, F[_], K[_], O](z: F[E], e: E => Machine[F, K, O]): Effect[F, K, O] =
      new Effect[F, K, O] {
        type Z = E

        val effect: F[Z] = z

        def apply(z: Z): Machine[F, K, O] = e(z)
      }
  }

  sealed trait Shift[+F[_], +K[_], +O] extends Machine[F, K, O] {
    def apply(): Machine[F, K, O]
  }
  object Shift {
    def apply[F[_], K[_], O](m: => Machine[F, K, O]): Shift[F, K, O] = new Shift[F, K, O] {
      def apply(): Machine[F, K, O] = m
    }
  }
}
