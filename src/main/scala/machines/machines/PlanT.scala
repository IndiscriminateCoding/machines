package machines.machines

import cats._
import cats.arrow.Category
import cats.implicits._
import machines.machines.PlanT._

trait PlanT[F[_], K[_], O, A] { outer =>
  def apply[R](
    done: A => F[R],
    emit: (O, F[R]) => F[R],
    await: Await[F, K, R],
    stop: F[R]
  ): F[R]

  def map[B](f: A => B): PlanT[F, K, O, B] = new PlanT[F, K, O, B] {
    def apply[R](d: B => F[R], e: (O, F[R]) => F[R], a: Await[F, K, R], s: F[R]): F[R] =
      outer(d compose f, e, a, s)
  }

  def flatMap[B](f: A => PlanT[F, K, O, B]): PlanT[F, K, O, B] = new PlanT[F, K, O, B] {
    def apply[R](d: B => F[R], e: (O, F[R]) => F[R], a: Await[F, K, R], s: F[R]): F[R] =
      outer(x => f(x)(d, e, a, s), e, a, s)
  }
}

object PlanT {
  trait Await[F[_], K[_], R] {
    def apply[Z](e: Z => F[R], k: K[Z], f: F[R]): F[R]
  }

  def stop[F[_], K[_], O, A]: PlanT[F, K, O, A] = new PlanT[F, K, O, A] {
    def apply[R](d: A => F[R], e: (O, F[R]) => F[R], a: Await[F, K, R], s: F[R]): F[R] = s
  }

  def pure[F[_], K[_], O, A](x: A): PlanT[F, K, O, A] = new PlanT[F, K, O, A] {
    def apply[R](d: A => F[R], e: (O, F[R]) => F[R], a: Await[F, K, R], f: F[R]): F[R] =
      d(x)
  }

  def lift[M[_] : Monad, K[_], O, A](m: M[A]): PlanT[M, K, O, A] = new PlanT[M, K, O, A] {
    def apply[R](d: A => M[R], e: (O, M[R]) => M[R], a: Await[M, K, R], s: M[R]): M[R] =
      m >>= d
  }

  def emit[F[_], K[_], O](o: O): PlanT[F, K, O, Unit] = new PlanT[F, K, O, Unit] {
    def apply[R](d: Unit => F[R], e: (O, F[R]) => F[R], a: Await[F, K, R], s: F[R]): F[R] =
      e(o, d(()))
  }

  def emitOpt[F[_], K[_], O](o: Option[O]): PlanT[F, K, O, Unit] =
    o.fold[PlanT[F, K, O, Unit]](stop)(emit)

  def awaits[F[_], K[_], O, I](k: K[I]): PlanT[F, K, O, I] = new PlanT[F, K, O, I] {
    def apply[R](d: I => F[R], e: (O, F[R]) => F[R], a: Await[F, K, R], s: F[R]): F[R] =
      a(d, k, s)
  }

  def await[F[_], K[_, _] : Category, O, I]: PlanT[F, K[I, ?], O, I] =
    new PlanT[F, K[I, ?], O, I] {
      def apply[R](d: I => F[R], e: (O, F[R]) => F[R], a: Await[F, K[I, ?], R], s: F[R]): F[R] =
        a(d, Category[K].id, s)
    }
}
