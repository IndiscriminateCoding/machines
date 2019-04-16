package machines

import machines.Machine._

object Process {
  def echo[F[_], A]: Process[F, A, A] = Await(Is.refl, (x: A) => Emit(x, echo), Stop)

  def filter[F[_], A](f: A => Boolean): Process[F, A, A] = Await(
    Is.refl,
    (x: A) => {
      val tail = filter(f)

      if (f(x)) Emit(x, tail)
      else tail
    },
    Stop
  )

  def take[F[_], A](n: Int): Process[F, A, A] =
    if (n <= 0) Stop
    else Await(Is.refl, (x: A) => Emit(x, take(n - 1)), Stop)

  def drop[F[_], A](n: Int): Process[F, A, A] =
    if (n <= 0) echo
    else Await(Is.refl, (_: A) => drop(n - 1), Stop)
}
