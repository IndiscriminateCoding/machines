package machines

import cats.arrow.Category
import machines.Machine._
import machines.Plan._

sealed trait Plan[+F[_], +K[_], +O, +A] { self =>
  protected def apply[G[a] >: F[a], N[a] >: K[a], E >: O](s: S[G, N, E, A]): Machine[G, N, E]

  final def map[B](f: A => B): Plan[F, K, O, B] = new Plan[F, K, O, B] {
    def apply[G[a] >: F[a], N[a] >: K[a], E >: O](s: S[G, N, E, B]): Machine[G, N, E] =
      self(new S[G, N, E, A] {
        def done(a: A): Machine[G, N, E] = s done f(a)

        def stop: Machine[G, N, E] = s.stop
      })
  }

  final def construct: Machine[F, K, O] = apply(new S[F, K, O, A] {
    def done(a: A): Machine[F, K, O] = stop

    val stop: Machine[F, K, O] = Stop
  })

  final def repeatedly: Machine[F, K, O] = apply(new S[F, K, O, A] {
    def done(a: A): Machine[F, K, O] = Shift(repeatedly)

    val stop: Machine[F, K, O] = Stop
  })
}

object Plan {
  private sealed trait S[+F[_], +K[_], +O, -A] {
    def done(a: A): Machine[F, K, O]

    def stop: Machine[F, K, O]
  }

  implicit class PlanOps[F[_], K[_], O, A](private val self: Plan[F, K, O, A]) extends AnyVal {
    def flatMap[B](f: A => Plan[F, K, O, B]): Plan[F, K, O, B] = new Plan[F, K, O, B] {
      def apply[G[a] >: F[a], N[a] >: K[a], E >: O](s: S[G, N, E, B]): Machine[G, N, E] =
        self(new S[G, N, E, A] {
          def done(a: A): Machine[G, N, E] = Shift(f(a)(s))

          def stop: Machine[G, N, E] = s.stop
        })
    }

    def liftMap[B](f: A => F[B]): Plan[F, K, O, B] = new Plan[F, K, O, B] {
      def apply[G[a] >: F[a], N[a] >: K[a], E >: O](s: S[G, N, E, B]): Machine[G, N, E] =
        self(new S[G, N, E, A] {
          def done(a: A): Machine[G, N, E] = Effect(f(a), s.done)

          def stop: Machine[G, N, E] = s.stop
        })
    }

    def orElse(p: Plan[F, K, O, A]): Plan[F, K, O, A] = new Plan[F, K, O, A] {
      def apply[G[a] >: F[a], N[a] >: K[a], E >: O](s: S[G, N, E, A]): Machine[G, N, E] =
        self(new S[G, N, E, A] {
          def done(a: A): Machine[G, N, E] = s done a

          def stop: Machine[G, N, E] = Shift(p(s))
        })
    }
  }

  val stop: Plan[Nothing, Nothing, Nothing, Nothing] =
    new Plan[Nothing, Nothing, Nothing, Nothing] {
      def apply[G[_] >: Nothing, N[_] >: Nothing, E >: Nothing](
        s: S[G, N, E, Nothing]
      ): Machine[G, N, E] = s.stop
    }

  def pure[A](a: A): Plan[Nothing, Nothing, Nothing, A] = new Plan[Nothing, Nothing, Nothing, A] {
    def apply[G[_] >: Nothing, N[_] >: Nothing, E >: Nothing](
      s: S[G, N, E, A]
    ): Machine[G, N, E] = s.done(a)
  }

  def emit[O](x: O): Plan[Nothing, Nothing, O, Unit] = new Plan[Nothing, Nothing, O, Unit] {
    def apply[G[_] >: Nothing, N[_] >: Nothing, E >: O](
      s: S[G, N, E, Unit]
    ): Machine[G, N, E] = Emit(x, s.done(()))
  }

  def emitOption[O](x: Option[O]): Plan[Nothing, Nothing, O, Unit] =
    x.fold[Plan[Nothing, Nothing, O, Unit]](stop)(emit)

  def awaits[K[_], A](f: K[A]): Plan[Nothing, K, Nothing, A] = new Plan[Nothing, K, Nothing, A] {
    def apply[G[_] >: Nothing, N[a] >: K[a], E >: Nothing](s: S[G, N, E, A]): Machine[G, N, E] =
      Await(f, s.done, s.stop)
  }

  def await[K[_, _], A](implicit K: Category[K]): Plan[Nothing, K[A, ?], Nothing, A] = awaits(K.id)

  def lift[F[_], A](eff: F[A]): Plan[F, Nothing, Nothing, A] = new Plan[F, Nothing, Nothing, A] {
    def apply[G[a] >: F[a], N[_] >: Nothing, E >: Nothing](s: S[G, N, E, A]): Machine[G, N, E] =
      Effect(eff, s.done)
  }

  def exhaust[F[_], O](f: F[Option[O]]): Plan[F, Nothing, O, Nothing] =
    lift(f)
      .flatMap(emitOption)
      .flatMap(_ => exhaust(f))
}
