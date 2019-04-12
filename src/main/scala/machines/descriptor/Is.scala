package machines.descriptor

import cats.arrow.Category

sealed trait Is[-A, +B] {
  def apply(x: A): B
}

object Is {
  private val reflAny = new Is[Any, Any] {
    def apply(x: Any): Any = x
  }

  def refl[A]: Is[A, A] = reflAny.asInstanceOf[Is[A, A]]

  implicit def isCategoryInstance: Category[Is] = new Category[Is] {
    def id[A]: Is[A, A] = refl

    def compose[A, B, C](f: Is[B, C], g: Is[A, B]): Is[A, C] = new Is[A, C] {
      def apply(x: A): C = f(g(x))
    }
  }
}
