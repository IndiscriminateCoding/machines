package machines

import cats.Eval
import cats.evidence.Is
import machines.Machine._
import org.scalatest.FlatSpec

class MachineSpec extends FlatSpec {
  it should "allow recursion when constructing Machines" in {
    def stream[F[_], K[_]](n: Int): Machine[K, F, Option[Int]] =
      if (n > 0) Emit(Some(1), Emit(None, Shift(stream(n - 1))))
      else Stop()

    stream[Eval, Int Is ?](1000 * 1000)
      .map(x => Eval.always(println(x)))
      .run_
      .value
  }
}
