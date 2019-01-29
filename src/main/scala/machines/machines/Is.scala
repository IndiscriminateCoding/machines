package machines.machines

import cats.arrow.Category

/* TODO: use cats.evidence instead? */
sealed trait Is[A, B] {
  def lr(a: A): B
  def rl(b: B): A
}

object Is {
  implicit val isCategoryInstance: Category[Is] = new Category[Is] {
    def id[A]: Is[A, A] = new Is[A, A] {
      @inline def lr(x: A): A = x

      @inline def rl(x: A): A = x
    }

    def compose[A, B, C](f: Is[B, C], g: Is[A, B]): Is[A, C] = new Is[A, C] {
      @inline def lr(a: A): C = f lr (g lr a)

      @inline def rl(b: C): A = g rl (f rl b)
    }
  }
}
