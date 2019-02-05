package machines.machines

import cats._
import cats.evidence.Is
import org.scalatest.FlatSpec

class PlanSpec extends FlatSpec {
  it should "produce stack-safe Machine using shift" ignore {
    def emit(from: Int): Plan[Int Is ?, Eval, String, Unit] = ((from match {
      case n if n > 0 => Plan.emit(n.toString)
      case _ => Plan.stop
    }): Plan[Int Is ?, Eval, String, Unit])
      .shift
      .flatMap(_ => emit(from - 1))

    emit(1000 * 1000)
      .construct
      .run(s => Eval.always(System.err.println(s)))
      .value
  }

  it should "keep stack-safety when using repeatedly (and implicit shift)" in {
    var cnt = 0
    val act = Plan.lift[Int Is ?, Eval, String, Int](Eval.always {
      cnt += 1
      cnt
    })

    val plan: Plan[Int Is ?, Eval, String, Unit] = act flatMap {
      case n if n < 1000 * 1000 * 10 => Plan.emit(n.toString)
      case _ => Plan.stop
    }

    plan
      .repeatedly
      .run(s => Eval.always(()))
      .value
  }
}