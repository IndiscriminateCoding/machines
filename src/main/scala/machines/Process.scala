package machines

import machines.Machine._
import machines.descriptor.Is

object Process {
  implicit class MachineOps[F[_], K[_], A](val self: Machine[F, K, A]) extends AnyVal {
    def ~>[B](p: Process[F, A, B]): Machine[F, K, B] = compose(self, p)
  }

  implicit class ProcessOps[F[_], A, B](val self: Process[F, A, B]) extends AnyVal {
    def <~[K[_]](m: Machine[F, K, A]): Machine[F, K, B] = compose(m, self)
  }

  def compose[F[_], K[_], A, B](m: Machine[F, K, A], p: Process[F, A, B]): Machine[F, K, B] =
    p match {
      case Stop => Stop
      case Emit(h, t) => Emit(h, compose(m, t))
      case awaitP: Await[F, A Is ?, B] => m match {
        case Stop => compose(Stop, awaitP.stop)
        case Emit(h, t) => compose(t, awaitP(awaitP.await(h)))
        case awaitM: Await[F, K, A] =>
          Await(awaitM.await, (a: awaitM.Z) => compose(awaitM(a), p), compose(awaitM.stop, p))
        case eff: Effect[F, K, A] => Effect(eff.effect, (e: eff.Z) => compose(eff(e), p))
        case shift: Shift[F, K, A] => Shift(compose(shift(), p))
      }
      case eff: Effect[F, A Is ?, B] => Effect(eff.effect, (e: eff.Z) => compose(m, eff(e)))
      case shift: Shift[F, A Is ?, B] => Shift(compose(m, shift()))
    }

  def echo[F[_], A]: Process[F, A, A] = Await(Is.refl, (x: A) => Emit(x, Shift(echo)), Stop)

  def filter[F[_], A](f: A => Boolean): Process[F, A, A] = Await(
    Is.refl,
    (x: A) => {
      val tail = Shift(filter(f))

      if (f(x)) Emit(x, tail)
      else tail
    },
    Stop
  )
}
