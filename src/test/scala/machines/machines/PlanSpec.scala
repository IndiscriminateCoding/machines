package machines.machines

import cats._
import cats.evidence.Is
import org.scalatest.FlatSpec

class PlanSpec extends FlatSpec {
  it should "be stack-safe" in {
    def loop(threshold: Int): Plan[? Is Int, Id, String, Unit] =
      Plan.awaits[? Is Int, Id, String, Int](Is.refl)
        .flatMap[Unit] {
          case n if n < threshold => Plan.emit[? Is Int, Id, String](n.toString)
          case _ => Plan.stop[? Is Int, Id, String, Unit]
        }
        .flatMap(_ => Plan.shift[? Is Int, Id, String])
        .flatMap(_ => loop(threshold))

    def done[A] = (_: A) => Eval.Unit
    val emit: (String, Eval[Unit]) => Eval[Unit] = {
      case (s, e) => Eval.always(System.err.println(s)).flatMap(_ => e)
    }
    var cnt = 0
    val await = new Plan.Await[? Is Int, Id, Eval[Unit]] {
      def await[Z](z: Z Is Int, e: Z => Eval[Unit], f: Eval[Unit]): Eval[Unit] = {
        cnt += 1
        e(z.flip coerce cnt)
      }

      def effect[Z](z: Id[Z], e: Z => Eval[Unit]): Eval[Unit] = e(z)

      def eval[Z](z: Eval[Z], e: Z => Eval[Unit]): Eval[Unit] = z flatMap e
    }
    val stop = Eval.Unit

    loop(1000 * 1000).apply(done, emit, await, stop).value
  }
}
