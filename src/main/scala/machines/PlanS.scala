package machines

import machines.Machine._

private[machines] sealed trait PlanS[+K[_], +F[_], +O, -A] {
  def done(a: A): Machine[K, F, O]

  def stop: Machine[K, F, O]
}

private[machines] object PlanS {
  final class Map[K[_], F[_], O, A, B](p: PlanS[K, F, O, B], f: A => B) extends PlanS[K, F, O, A] {
    def done(a: A): Machine[K, F, O] = p done f(a)

    def stop: Machine[K, F, O] = p.stop
  }

  final class FlatMap[K[_], F[_], O, A, B](p: PlanS[K, F, O, B], f: A => Plan[K, F, O, B])
    extends PlanS[K, F, O, A] {
    def done(a: A): Machine[K, F, O] = Shift(f(a).apply(p))

    def stop: Machine[K, F, O] = p.stop
  }

  final class LiftMap[K[_], F[_], O, A, B](p: PlanS[K, F, O, B], f: A => F[B])
    extends PlanS[K, F, O, A] {
    def done(a: A): Machine[K, F, O] = Effect(f(a), p.done)

    def stop: Machine[K, F, O] = p.stop
  }

  final class Combine[K[_], F[_], O, A](s: PlanS[K, F, O, A], p: Plan[K, F, O, A])
    extends PlanS[K, F, O, A] {
    def done(a: A): Machine[K, F, O] = s.done(a)

    val stop: Machine[K, F, O] = Shift(p(s))
  }

  final class Construct[K[_], F[_], O, A](tail: => Machine[K, F, O]) extends PlanS[K, F, O, A] {
    def done(a: A): Machine[K, F, O] = Shift(tail)

    val stop: Machine[K, F, O] = Stop
  }
}
