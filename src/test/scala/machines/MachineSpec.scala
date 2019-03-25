package machines

import cats.Eval
import cats.evidence.Is
import machines.Machine._
import org.scalatest.FlatSpec

class MachineSpec extends FlatSpec {
  it should "allow recursion when constructing Machines" in {
    def stream[F[_], K[_]]: Machine[K, F, Option[Int]] =
      Emit(Some(1), Emit(None, stream))

    stream[Eval, Int Is ?].run(s => Eval.always(println(s))).value
  }
}
