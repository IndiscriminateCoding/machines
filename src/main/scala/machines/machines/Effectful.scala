package machines.machines

import cats.~>

trait Effectful[E[_], F[_]] extends (E ~> F)

object Effectful {
  def apply[E[_], F[_]](implicit E: Effectful[E, F]): Effectful[E, F] = E
}
