package machines

import cats.{ Monad, Monoid }

sealed trait Machine[K[_], F[_], O] {
  def foldMap[R](f: O => F[R])(implicit F: Monad[F], M: Monoid[R]): F[R]

  def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit]
}

object Machine {
  class Stop[K[_], F[_], O] extends Machine[K, F, O] {
    final def foldMap[R](f: O => F[R])(implicit F: Monad[F], M: Monoid[R]): F[R] = F.pure(M.empty)
    final def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit] = F.unit
  }
  object Stop {
    @inline def apply[K[_], F[_], O](): Stop[K, F, O] = new Stop
  }

  class Emit[K[_], F[_], O](h: O, t: Machine[K, F, O]) extends Machine[K, F, O] {
    final def foldMap[R](f: O => F[R])(implicit F: Monad[F], M: Monoid[R]): F[R] =
      F.flatMap(f(h))(r => F.map(t.foldMap(f))(M.combine(r, _)))
    final def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit] = F.flatMap(f(h))(_ => t.run(f))
  }
  object Emit {
    @inline def apply[K[_], F[_], O](h: O, t: Machine[K, F, O]): Emit[K, F, O] = new Emit(h, t)
  }

  trait Await[K[_], F[_], O] extends Machine[K, F, O] {
    type Z

    def await: K[Z]
    def apply(z: Z): Machine[K, F, O]
    def stop: Machine[K, F, O]

    final def foldMap[R](f: O => F[R])(implicit F: Monad[F], M: Monoid[R]): F[R] = stop.foldMap(f)
    final def run(g: O => F[Unit])(implicit F: Monad[F]): F[Unit] = stop.run(g)
  }
  object Await {
    @inline def apply[E, K[_], F[_], O](
      z: K[E],
      e: E => Machine[K, F, O],
      f: Machine[K, F, O]
    ): Await[K, F, O] = new Await[K, F, O] {
      type Z = E

      val await: K[Z] = z

      def apply(z: Z): Machine[K, F, O] = e(z)

      val stop: Machine[K, F, O] = f
    }
  }

  trait Effect[K[_], F[_], O] extends Machine[K, F, O] {
    type Z

    def effect: F[Z]
    def apply(z: Z): Machine[K, F, O]

    final def foldMap[R](f: O => F[R])(implicit F: Monad[F], M: Monoid[R]): F[R] =
      F.flatMap(effect)(z => apply(z).foldMap(f))
    final def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit] =
      F.flatMap(effect)(z => apply(z).run(f))
  }
  object Effect {
    @inline def apply[E, K[_], F[_], O](z: F[E], e: E => Machine[K, F, O]): Effect[K, F, O] =
      new Effect[K, F, O] {
        type Z = E

        val effect: F[Z] = z

        def apply(z: Z): Machine[K, F, O] = e(z)
      }
  }

  trait Shift[K[_], F[_], O] extends Machine[K, F, O] {
    def apply(): Machine[K, F, O]

    final def foldMap[R](f: O => F[R])(implicit F: Monad[F], M: Monoid[R]): F[R] =
      apply().foldMap(f)
    final def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit] = apply().run(f)
  }
  object Shift {
    @inline def apply[K[_], F[_], O](@inline m: => Machine[K, F, O]): Shift[K, F, O] = () => m
  }
}
