package machines.machines

import cats._

sealed trait Machine[K[_], F[_], O] {
  def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit]
}

object Machine {
  private[machines] class Stop[K[_], F[_], O] extends Machine[K, F, O] {
    final def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit] = F.unit
  }

  private[machines] class Emit[K[_], F[_], O](h: O, t: Machine[K, F, O])
    extends Machine[K, F, O] {
    final def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit] = F.flatMap(f(h))(_ => t.run(f))
  }

  private[machines] trait Await[K[_], F[_], O] extends Machine[K, F, O] {
    type Z

    def await: K[Z]
    def apply(z: Z): Machine[K, F, O]
    def stop: Machine[K, F, O]

    final def run(g: O => F[Unit])(implicit F: Monad[F]): F[Unit] = stop.run(g)
  }

  private[machines] trait Effect[K[_], F[_], O] extends Machine[K, F, O] {
    type Z

    def effect: F[Z]
    def apply(z: Z): Machine[K, F, O]

    final def run(f: O => F[Unit])(implicit F: Monad[F]): F[Unit] =
      F.flatMap(effect)(z => apply(z).run(f))
  }
}
