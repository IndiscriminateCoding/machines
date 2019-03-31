package machines

import cats._
import cats.arrow.Category
import machines.Machine._

sealed trait Plan[+K[_], +F[_], +O, +A] {
  private[machines]
  def apply[N[a] >: K[a], G[a] >: F[a], E >: O](s: PlanS[N, G, E, A]): Machine[N, G, E]

  final def construct: Machine[K, F, O] = apply(new PlanS.Construct(Stop))

  final def repeatedly: Machine[K, F, O] = apply(new PlanS.Construct(repeatedly))
}

object Plan {
  implicit class RichPlan[K[_], F[_], O, A](private val self: Plan[K, F, O, A]) extends AnyVal {
    def map[B](f: A => B): Plan[K, F, O, B] = new Plan[K, F, O, B] {
      def apply[N[a] >: K[a], G[a] >: F[a], E >: O](s: PlanS[N, G, E, B]): Machine[N, G, E] =
        self(new PlanS.Map(s, f))
    }

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

  def stop[K[_], F[_], O, A]: Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[N[a] >: K[a], G[a] >: F[a], E >: O](s: PlanS[N, G, E, A]): Machine[N, G, E] = s.stop
  }

  def pure[K[_], F[_], O, A](a: A): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[N[a] >: K[a], G[a] >: F[a], E >: O](s: PlanS[N, G, E, A]): Machine[N, G, E] =
      s.done(a)
  }

  def emit[K[_], F[_], O](x: O): Plan[K, F, O, Unit] = new Plan[K, F, O, Unit] {
    def apply[N[a] >: K[a], G[a] >: F[a], E >: O](s: PlanS[N, G, E, Unit]): Machine[N, G, E] =
      Emit(x, s.done(()))
  }

  def emitOption[K[_], F[_], O](x: Option[O]): Plan[K, F, O, Unit] =
    x.fold[Plan[K, F, O, Unit]](stop)(emit)

  def awaits[K[_], F[_], O, A](f: K[A]): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[N[a] >: K[a], G[a] >: F[a], E >: O](s: PlanS[N, G, E, A]): Machine[N, G, E] =
      Await(f, s.done, s.stop)
  }

  def await[K[_, _], F[_], O, A](implicit K: Category[K]): Plan[K[A, ?], F, O, A] =
    awaits[K[A, ?], F, O, A](K.id)

  def lift[K[_], F[_], O, A](eff: F[A]): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[N[a] >: K[a], G[a] >: F[a], E >: O](s: PlanS[N, G, E, A]): Machine[N, G, E] =
      Effect(eff, s.done)
  }

  def exhaust[K[_], F[_], O](f: F[Option[O]])(implicit F: Monad[F]): Plan[K, F, O, Unit] =
    lift[K, F, O, Option[O]](f)
      .flatMap(emitOption)
      .flatMap(_ => exhaust(f))
}
