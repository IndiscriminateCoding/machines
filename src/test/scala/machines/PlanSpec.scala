package machines

import cats._
import cats.evidence.Is
import cats.implicits._
import org.scalatest.FlatSpec

class PlanSpec extends FlatSpec {
  it should "produce stack-safe Machine using shift" in {
    def emit(from: Int): Plan[Int Is ?, Eval, Int, Unit] = ((from match {
      case n if n > 0 => Plan.emit(n + 42)
      case _ => Plan.stop
    }): Plan[Int Is ?, Eval, Int, Unit])
      .shift
      .flatMap(Plan.pure)
      .flatMap(Plan.pure)
      .flatMap(Plan.pure)
      .flatMap(Plan.pure)
      .flatMap(_ => emit(from - 1))

    var res = 0
    /*    emit(100 * 1000 * 1000)
          .construct
          .run(s => Eval.always(res += s))
          .value */
    emit(10 * 1000 * 1000)
      .construct
      .foldMap(x => Eval.now(res += x))
      .value
    println(res)
  }

  it should "keep stack-safety when using repeatedly (and implicit shift)" ignore {
    var cnt = 0
    val act = Plan.lift[Int Is ?, Eval, String, Int](Eval.always {
      cnt += 1
      cnt
    })

    val plan: Plan[Int Is ?, Eval, String, Unit] = act flatMap {
      case n if n < 1000 * 1000 => Plan.emit(n.toString)
      case _ => Plan.stop
    }

    plan
      .repeatedly
      .run(s => Eval.always(System.err.println(s)))
      .value
  }

  it should "keep stack-safety when using exhaust" ignore {
    var cnt = 0
    val act = Eval.always {
      cnt += 1
      if (cnt < 1000 * 1000) Some(cnt.toString)
      else None
    }

    val plan = Plan.exhaust[Nothing, Eval, String](act)

    plan
      .construct
      .run(s => Eval.always(System.err.println(s)))
      .value
  }
}
