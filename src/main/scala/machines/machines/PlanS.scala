package machines.machines

import cats.Eval

trait PlanS[K[_], F[_], O, A, R] {
  def done(a: A): R

  def emit(o: O, r: R): R

  def await[Z](z: K[Z], e: Z => R, f: R): R

  def effect[Z](z: F[Z], e: Z => R): R

  def eval[Z](z: Eval[Z], e: Z => R): R

  def stop: R
}

object PlanS {
  private[machines] sealed abstract class OverrideDone[K[_], F[_], O, A, B, R](
    p: PlanS[K, F, O, B, R]
  ) extends PlanS[K, F, O, A, R] {
    def emit(o: O, r: R): R = p.emit(o, r)

    def await[Z](z: K[Z], e: Z => R, f: R): R = p.await(z, e, f)

    def effect[Z](z: F[Z], e: Z => R): R = p.effect(z, e)

    def eval[Z](z: Eval[Z], e: Z => R): R = p.eval(z, e)

    def stop: R = p.stop
  }

  private[machines] final class Map[K[_], F[_], O, A, B, R](
    p: PlanS[K, F, O, B, R],
    f: A => B
  ) extends OverrideDone[K, F, O, A, B, R](p) {
    def done(a: A): R = p done f(a)
  }

  private[machines] final class FlatMap[K[_], F[_], O, A, B, R](
    p: PlanS[K, F, O, B, R],
    f: A => Plan[K, F, O, B]
  ) extends OverrideDone[K, F, O, A, B, R](p) {
    def done(a: A): R = f(a)(p)
  }

  private[machines] final class LiftMap[K[_], F[_], O, A, B, R](
    p: PlanS[K, F, O, B, R],
    f: A => F[B]
  ) extends OverrideDone[K, F, O, A, B, R](p) {
    def done(a: A): R = p.effect(f(a), p.done)
  }
}
