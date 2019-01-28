package machines.machines

import cats._
import machines.machines.Is._
import machines.machines.PlanT.Await
import org.scalatest.FlatSpec

class PlanTSpec extends FlatSpec {
  it should "be stack-safe" in {
    def inf[F[_]]: PlanT[F, Is[Int, ?], String, Unit] =
      PlanT.await
        .flatMap { x: Int => PlanT.emit[F, Is[Int, ?], String](x.toString) }
        .flatMap { _ => inf }

    def done[A] = (_: A) => ()

    val emit: (String, Id[Unit]) => Id[Unit] = {
      case (s, _) => println(s)
    }
    val await = new Await[Id, Is[Int, ?], Unit] {
      def apply[Z](e: Z => Id[Unit], k: Is[Int, Z], f: Id[Unit]): Id[Unit] = e(k lr 1)
    }
    val stop: Id[Unit] = ()

    inf[Id](done, emit, await, stop)
  }
}
