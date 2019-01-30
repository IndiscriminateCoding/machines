package machines.machines

import cats._
import machines.machines.Is._
import machines.machines.PlanT.Await
import org.scalatest.FlatSpec

class PlanTSpec extends FlatSpec {
  it should "be stack-safe" in {
    def loop[F[_]](threshold: Int)(implicit F: Monad[F]): PlanT[F, Is[Int, ?], String, Unit] =
      PlanT.await[F, Is, String, Int]
        .flatMap {
          case n if n < threshold => PlanT.emit[F, Is[Int, ?], String](n.toString)
          case _ => PlanT.stop[F, Is[Int, ?], String, Unit]
        }
        .flatMap { _ => loop(threshold)(F) }
        .shift

    def done[A] = (_: A) => Eval.now(())

    val emit: (String, Eval[Unit]) => Eval[Unit] = {
      case (s, e) => Eval.always(System.err.println(s)).flatMap(_ => e)
    }
    var cnt = 0
    val await = new Await[Eval, Is[Int, ?], Unit] {
      def apply[Z](e: Z => Eval[Unit], k: Is[Int, Z], f: Eval[Unit]): Eval[Unit] = {
        cnt += 1
        e(k lr cnt)
      }
    }
    val stop: Eval[Unit] = Eval.now(())

    loop[Eval](1000 * 1000)(implicitly[Monad[Eval]])(done, emit, await, stop).value
  }
}
