package machines.machines

import cats._
import cats.evidence.Is
import org.scalatest.FlatSpec

class PlanSpec extends FlatSpec {
  it should "be stack-safe" in {
    def loop[F[_]](threshold: Int)(implicit F: Monad[F]): Plan[Int Is ?, F, String, Unit] =
      Plan.awaits[Int Is ?, F, String, Int](Is.refl)
        .flatMap[Unit] {
          case n if n < threshold => Plan.emit[Int Is ?, F, String](n.toString)
          case _ => Plan.stop[Int Is ?, F, String, Unit]
        }
        .flatMap(_ => loop(threshold)(F))

    def done[A] = (_: A) => Eval.now(())
    val emit: (String, Eval[Unit]) => Eval[Unit] = {
      case (s, e) => Eval.always(System.err.println(s)).flatMap(_ => e)
    }
    var cnt = 0
    val await = new Plan.Await[Int Is ?, Eval[Unit]] {
      def apply[Z](z: Is[Int, Z], e: Z => Eval[Unit], f: Eval[Unit]): Eval[Unit] = {
        cnt += 1
        e(z coerce cnt)
      }
    }
    val stop: Eval[Unit] = Eval.Unit

    loop[Eval](1000 * 1000).apply(done, emit, await, stop).value
  }
}
