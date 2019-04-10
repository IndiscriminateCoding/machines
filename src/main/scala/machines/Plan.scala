package machines

import cats.arrow.Category
import machines.Machine._

sealed trait Plan[+K[_], +F[_], +O, +A] { self =>
  private[machines]
  def apply[N[a] >: K[a], G[a] >: F[a], E >: O](s: PlanS[N, G, E, A]): Machine[N, G, E]

  final def map[B](f: A => B): Plan[K, F, O, B] = new Plan[K, F, O, B] {
    def apply[N[a] >: K[a], G[a] >: F[a], E >: O](s: PlanS[N, G, E, B]): Machine[N, G, E] =
      self(new PlanS.Map(s, f))
  }

  final def construct: Machine[K, F, O] = apply(new PlanS.Construct(Stop))

  final def repeatedly: Machine[K, F, O] = apply(new PlanS.Construct(repeatedly))
}

object Plan {
  implicit class PlanOps[K[_], F[_], O, A](private val self: Plan[K, F, O, A]) extends AnyVal {
    def flatMap[B](f: A => Plan[K, F, O, B]): Plan[K, F, O, B] = new Plan[K, F, O, B] {
      def apply[N[a] >: K[a], G[a] >: F[a], E >: O](s: PlanS[N, G, E, B]): Machine[N, G, E] =
        self(new PlanS.FlatMap(s, f))
    }

    def liftMap[B](f: A => F[B]): Plan[K, F, O, B] = new Plan[K, F, O, B] {
      def apply[N[a] >: K[a], G[a] >: F[a], E >: O](s: PlanS[N, G, E, B]): Machine[N, G, E] =
        self(new PlanS.LiftMap(s, f))
    }

    def combine(p: Plan[K, F, O, A]): Plan[K, F, O, A] = new Plan[K, F, O, A] {
      def apply[N[a] >: K[a], G[a] >: F[a], E >: O](s: PlanS[N, G, E, A]): Machine[N, G, E] =
        self(new PlanS.Combine(s, p))
    }
  }

  val stop: Plan[Nothing, Nothing, Nothing, Nothing] =
    new Plan[Nothing, Nothing, Nothing, Nothing] {
      def apply[N[_] >: Nothing, G[_] >: Nothing, E >: Nothing](
        s: PlanS[N, G, E, Nothing]
      ): Machine[N, G, E] = s.stop
    }

  def pure[A](a: A): Plan[Nothing, Nothing, Nothing, A] = new Plan[Nothing, Nothing, Nothing, A] {
    def apply[N[_] >: Nothing, G[_] >: Nothing, E >: Nothing](
      s: PlanS[N, G, E, A]
    ): Machine[N, G, E] = s.done(a)
  }

  def emit[O](x: O): Plan[Nothing, Nothing, O, Unit] = new Plan[Nothing, Nothing, O, Unit] {
    def apply[N[_] >: Nothing, G[_] >: Nothing, E >: O](
      s: PlanS[N, G, E, Unit]
    ): Machine[N, G, E] = Emit(x, s.done(()))
  }

  def emitOption[O](x: Option[O]): Plan[Nothing, Nothing, O, Unit] =
    x.fold[Plan[Nothing, Nothing, O, Unit]](stop)(emit)

  def awaits[K[_], A](f: K[A]): Plan[K, Nothing, Nothing, A] = new Plan[K, Nothing, Nothing, A] {
    def apply[N[a] >: K[a], G[_] >: Nothing, E >: Nothing](s: PlanS[N, G, E, A]): Machine[N, G, E] =
      Await(f, s.done, s.stop)
  }

  def await[K[_, _], A](implicit K: Category[K]): Plan[K[A, ?], Nothing, Nothing, A] = awaits(K.id)

  def lift[F[_], A](eff: F[A]): Plan[Nothing, F, Nothing, A] = new Plan[Nothing, F, Nothing, A] {
    def apply[N[_] >: Nothing, G[a] >: F[a], E >: Nothing](s: PlanS[N, G, E, A]): Machine[N, G, E] =
      Effect(eff, s.done)
  }

  def exhaust[F[_], O](f: F[Option[O]]): Plan[Nothing, F, O, Nothing] =
    lift(f)
      .flatMap(emitOption)
      .flatMap(_ => exhaust(f))
}
