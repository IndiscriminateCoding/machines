package machines.input

sealed trait Is[-A, +B] {
  def apply(x: A): B
}

object Is {
  private val reflAny = new Is[Any, Any] {
    def apply(x: Any): Any = x
  }

  def refl[A]: Is[A, A] = reflAny.asInstanceOf[Is[A, A]]
}
