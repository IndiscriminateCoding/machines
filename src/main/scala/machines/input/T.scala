package machines.input

sealed trait T[-L, -R, +A]

object T {
  sealed trait L[-L, +A] extends T[L, Any, A] {
    def apply(l: L): A
  }
  object L {
    def apply[A, B]: T[A, B, A] = new L[A, A] {
      def apply(l: A): A = l
    }
  }

  sealed trait R[-R, +A] extends T[Any, R, A] {
    def apply(r: R): A
  }
  object R {
    def apply[A, B]: T[A, B, B] = new R[B, B] {
      def apply(r: B): B = r
    }
  }
}
