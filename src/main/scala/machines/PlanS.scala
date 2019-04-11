package machines

import machines.Machine._

private[machines] sealed trait PlanS[+F[_], +K[_], +O, -A] {
  def done(a: A): Machine[F, K, O]

  def stop: Machine[F, K, O]
}

private[machines] object PlanS {
  final class Map[F[_], K[_], O, A, B](p: PlanS[F, K, O, B], f: A => B) extends PlanS[F, K, O, A] {
    def done(a: A): Machine[F, K, O] = p done f(a)

    def stop: Machine[F, K, O] = p.stop
  }

  final class FlatMap[F[_], K[_], O, A, B](p: PlanS[F, K, O, B], f: A => Plan[F, K, O, B])
    extends PlanS[F, K, O, A] {
    def done(a: A): Machine[F, K, O] = Shift(f(a)(p))

    def stop: Machine[F, K, O] = p.stop
  }

  final class LiftMap[F[_], K[_], O, A, B](
    p: PlanS[F, K, O, B], f: A => F[B]
  ) extends PlanS[F, K, O, A] {
    def done(a: A): Machine[F, K, O] = Effect(f(a), p.done)

    def stop: Machine[F, K, O] = p.stop
  }

  final class OrElse[F[_], K[_], O, A](
    s: PlanS[F, K, O, A],
    p: Plan[F, K, O, A]
  ) extends PlanS[F, K, O, A] {
    def done(a: A): Machine[F, K, O] = s.done(a)

    val stop: Machine[F, K, O] = Shift(p(s))
  }

  final class Construct[F[_], K[_], O, A](tail: Machine[F, K, O]) extends PlanS[F, K, O, A] {
    def done(a: A): Machine[F, K, O] = tail

    val stop: Machine[F, K, O] = Stop
  }
}
