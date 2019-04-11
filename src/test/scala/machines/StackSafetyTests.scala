package machines

import cats._
import machines.Machine._
import machines.descriptor.Is
import org.scalatest.FlatSpec

class StackSafetyTests extends FlatSpec {
  val length: Int = 1000 * 1000

  it should "allow recursion when constructing Machines" in {
    def stream[F[_], K[_]](n: Int): Machine[F, K, Option[Int]] =
      if (n > 0) Emit(Some(1), Emit(None, Shift(stream(n - 1))))
      else Stop

    stream[Eval, Int Is ?](length)
      .run_
      .value
  }

  it should "produce stack-safe Machine" in {
    def emit(from: Int): Plan[Eval, Int Is ?, Int, Unit] = (from match {
      case n if n > 0 => Plan.emit(n + 42)
      case _ => Plan.stop
    })
      .flatMap(Plan.pure)
      .flatMap(_ => emit(from - 1))

    val machine = emit(length).construct
    var res = 0
    res = machine
      .foldLeft(0)((a, n) => n + a)
      .value
    machine
      .map { o => res += o }
      .run_
      .value
    println(res)
  }

  it should "keep stack-safety when using repeatedly" in {
    var cnt = 0
    val act = Plan.lift(Eval.always {
      cnt += 1
      cnt
    })

    val plan = act flatMap {
      case n if n < length => Plan.emit(n.toString)
      case _ => Plan.stop
    }

    plan
      .repeatedly
      .map(identity)
      .run_
      .value
  }

  it should "keep stack-safety when using exhaust" in {
    var cnt = 0
    val act = Eval.always {
      cnt += 1
      if (cnt < length) Some(cnt.toString)
      else None
    }

    val plan = Plan.exhaust(act)

    plan
      .construct
      .map(identity)
      .run_
      .value
  }
}