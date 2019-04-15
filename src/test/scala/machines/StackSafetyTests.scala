package machines

import cats._
import machines.Machine._
import org.scalatest.{ FlatSpec, Matchers }

class StackSafetyTests extends FlatSpec with Matchers {
  val len: Int = 1000 * 1000

  it should "allow recursion when constructing Machines" in {
    def stream[F[_], K[_]](n: Int): Machine[F, K, Option[Int]] =
      if (n > 0) Emit(Some(1), Emit(None, Shift(stream(n - 1))))
      else Stop

    stream[Eval, Int Is ?](len)
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

    val machine = emit(len).construct
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
    Plan.emit(1)
      .repeatedly
      .through(Process.take(len))
      .map(identity)
      .run_(implicitly[Monad[Eval]])
      .value
  }

  it should "keep stack-safety when using exhaust" in {
    var cnt = 0
    val act = Eval.always {
      cnt += 1
      if (cnt < len) Some(cnt.toString)
      else None
    }

    val plan = Plan.exhaust(act)

    plan
      .construct
      .map(identity)
      .run_
      .value
  }

  it should "be stack-safe to use various Process functions" in {
    var cnt = 0
    val act = Eval.always { cnt += 1 }

    (for { _ <- Plan.lift(act); _ <- Plan.emit(()) } yield ())
      .repeatedly
      .through(Process.take(len))
      .through(Process.echo)
      .through(Process.drop(123))
      .through(Process.filter(_ => true))
      .run_
      .value

    cnt shouldBe len
  }
}
