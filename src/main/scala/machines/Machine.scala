package machines

import cats._

sealed trait Machine[K[_], F[_], O] {
  def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit]
}

object Machine {
  class Stop[K[_], F[_], O] extends Machine[K, F, O] {
    final def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit] = F.unit
  }
  object Stop {
    def apply[K[_], F[_], O](): Stop[K, F, O] = new Stop
  }

  class Emit[K[_], F[_], O](h: O, t: => Machine[K, F, O])
    extends Machine[K, F, O] {
    final def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit] = F.flatMap(f(h))(_ => t.run(f))
  }
  object Emit {
    def apply[K[_], F[_], O](h: O, t: => Machine[K, F, O]): Emit[K, F, O] = new Emit(h, t)
  }

  trait Await[K[_], F[_], O] extends Machine[K, F, O] {
    type Z

    def await: K[Z]
    def apply(z: Z): Machine[K, F, O]
    def stop: Machine[K, F, O]

    final def run(g: O => F[Unit])(implicit F: Monad[F]): F[Unit] = stop.run(g)
  }
  object Await {
    def apply[E, K[_], F[_], O](
      z: K[E],
      e: E => Machine[K, F, O],
      f: Machine[K, F, O]
    ): Await[K, F, O] = new Await[K, F, O] {
      type Z = E

      def await: K[Z] = z

      def apply(z: Z): Machine[K, F, O] = e(z)

      def stop: Machine[K, F, O] = f
    }
  }

  trait Effect[K[_], F[_], O] extends Machine[K, F, O] {
    type Z

    def effect: F[Z]
    def apply(z: Z): Machine[K, F, O]

    final def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit] =
      F.flatMap(effect)(z => apply(z).run(f))
  }
  object Effect {
    def apply[E, K[_], F[_], O](z: F[E], e: E => Machine[K, F, O]): Effect[K, F, O] =
      new Effect[K, F, O] {
        type Z = E

        def effect: F[Z] = z

        def apply(z: Z): Machine[K, F, O] = e(z)
      }
  }
}
