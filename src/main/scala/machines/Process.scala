package machines

import machines.Machine._
import machines.descriptor.Is

object Process {
  implicit class MachineOps[K[_], F[_], A](val self: Machine[K, F, A]) extends AnyVal {
    def ~>[B](p: Process[A, F, B]): Machine[K, F, B] = compose(self, p)
  }

  implicit class ProcessOps[F[_], A, B](val self: Process[A, F, B]) extends AnyVal {
    def <~[K[_]](m: Machine[K, F, A]): Machine[K, F, B] = compose(m, self)
  }

  def compose[K[_], F[_], A, B](m: Machine[K, F, A], p: Process[A, F, B]): Machine[K, F, B] =
    p match {
      case Stop => Stop
      case Emit(h, t) => Emit(h, compose(m, t))
      case awaitP: Await[A Is ?, F, B] => m match {
        case Stop => compose(Stop, awaitP.stop)
        case Emit(h, t) => compose(t, awaitP(awaitP.await(h)))
        case awaitM: Await[K, F, A] =>
          Await(awaitM.await, (a: awaitM.Z) => compose(awaitM(a), p), compose(awaitM.stop, p))
        case eff: Effect[K, F, A] => Effect(eff.effect, (e: eff.Z) => compose(eff(e), p))
        case shift: Shift[K, F, A] => Shift(compose(shift(), p))
      }
      case eff: Effect[A Is ?, F, B] => Effect(eff.effect, (e: eff.Z) => compose(m, eff(e)))
      case shift: Shift[A Is ?, F, B] => Shift(compose(m, shift()))
    }

  def echo[F[_], A]: Process[A, F, A] = Await(Is.refl, (x: A) => Emit(x, Shift(echo)), Stop)

  def filter[F[_], A](f: A => Boolean): Process[A, F, A] = Await(
    Is.refl,
    (x: A) => {
      val tail = Shift(filter(f))

      if (f(x)) Emit(x, tail)
      else tail
    },
    Stop
  )
}
