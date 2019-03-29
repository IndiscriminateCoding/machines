package machines

import cats.Eval
import cats.evidence.Is
import cats.implicits._
import machines.Machine._
import org.scalatest.FlatSpec

class MachineSpec extends FlatSpec {
  it should "allow recursion when constructing Machines" in {
    def stream[F[_], K[_]]: Machine[K, F, Option[Int]] =
      Emit(Some(1), Emit(None, Shift(stream)))

    stream[Eval, Int Is ?].foldMap(x => Eval.always(println(x))).value
  }
}
