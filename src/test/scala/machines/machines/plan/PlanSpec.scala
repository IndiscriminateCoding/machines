package machines.machines.plan

import cats._
import cats.evidence.Is
import org.scalatest.FlatSpec

class PlanSpec extends FlatSpec {
  private[this] val sym: PlanS[Int Is ?, Id, String, Unit, Eval[Unit]] =
    new PlanS[Int Is ?, Id, String, Unit, Eval[Unit]] {
      private[this] var cnt = 0

      def done(a: Unit): Eval[Unit] = Eval.Unit

      def emit(o: String, r: Eval[Unit]): Eval[Unit] =
        Eval.always(System.err.println(o)).flatMap(_ => r)

      def await[Z](z: Int Is Z, e: Z => Eval[Unit], f: Eval[Unit]): Eval[Unit] =
        Eval.always(cnt += 1).flatMap(_ => e(z coerce cnt))

      def effect[Z](z: Id[Z], e: Z => Eval[Unit]): Eval[Unit] = e(z)

      def eval[Z](z: Eval[Z], e: Z => Eval[Unit]): Eval[Unit] = z flatMap e

      def stop: Eval[Unit] = Eval.Unit
    }

  it should "keep stack-safety of target type" in {
    def loop(threshold: Int): Plan[Int Is ?, Id, String, Unit] =
      Plan.awaits[Int Is ?, Id, String, Int](Is.refl)
        .flatMap[Unit] {
          case n if n < threshold => Plan.emit(n.toString)
          case _ => Plan.stop
        }
        .flatMap(_ => loop(threshold))

    loop(1000 * 1000)(sym).value
  }

  it should "be stack-safe on long Emit-only chains with Plan.shift" in {
    def emit(from: Int): Plan[Int Is ?, Id, String, Unit] = ((from match {
      case n if n > 0 => Plan.emit(n.toString)
      case _ => Plan.stop
    }) : Plan[Int Is ?, Id, String, Unit])
    .flatMap(_ => Plan.shift)
    .flatMap(_ => emit(from - 1))

    emit(1000 * 1000)(sym).value
  }
}
