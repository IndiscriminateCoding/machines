package machines

import machines.input._

object Tee {
  def zip[A, B]: Tee[Nothing, A, B, (A, B)] = (for {
    l <- Plan awaits T.L[A, B]
    r <- Plan awaits T.R[A, B]
  } yield l -> r).repeatedly
}
