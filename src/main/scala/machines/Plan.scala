package machines

import cats._
import cats.arrow.Category
import machines.Machine._

sealed trait Plan[K[_], F[_], O, A] { outer =>
  def apply[R](sym: PlanS[K, F, O, A, R]): R

  def map[B](f: A => B): Plan[K, F, O, B] = new Plan[K, F, O, B] {
    def apply[R](s: PlanS[K, F, O, B, R]): R = outer(new PlanS.Map(s, f))
  }

  def flatMap[B](f: A => Plan[K, F, O, B]): Plan[K, F, O, B] = new Plan[K, F, O, B] {
    def apply[R](s: PlanS[K, F, O, B, R]): R = outer(new PlanS.FlatMap(s, f))
  }

  def liftMap[B](f: A => F[B]): Plan[K, F, O, B] = new Plan[K, F, O, B] {
    def apply[R](s: PlanS[K, F, O, B, R]): R = outer(new PlanS.LiftMap(s, f))
  }

  def shift(implicit F: Applicative[F]): Plan[K, F, O, A] = liftMap(F.pure)

  def combine(p: Plan[K, F, O, A]): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](s: PlanS[K, F, O, A, R]): R = outer(new PlanS.Combine(s, p))
  }

  def construct: Machine[K, F, O] = apply(new PlanS.Construct(new Stop))

  def repeatedly: Machine[K, F, O] = apply(new PlanS.Construct(repeatedly))
}

object Plan {
  def stop[K[_], F[_], O, A]: Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](sym: PlanS[K, F, O, A, R]): R = sym.stop
  }

  def pure[K[_], F[_], O, A](a: A): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](sym: PlanS[K, F, O, A, R]): R = sym.done(a)
  }

  def emit[K[_], F[_], O](x: O): Plan[K, F, O, Unit] = new Plan[K, F, O, Unit] {
    def apply[R](sym: PlanS[K, F, O, Unit, R]): R = sym.emit(x, sym.done(()))
  }

  def emitOption[K[_], F[_], O](x: Option[O]): Plan[K, F, O, Unit] =
    x.fold[Plan[K, F, O, Unit]](stop)(emit)

  def awaits[K[_], F[_], O, A](f: K[A]): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](sym: PlanS[K, F, O, A, R]): R = sym.await(f, sym.done, sym.stop)
  }

  def await[K[_, _], F[_], O, A](implicit K: Category[K]): Plan[K[A, ?], F, O, A] =
    awaits[K[A, ?], F, O, A](K.id)

  def lift[K[_], F[_], O, A](eff: F[A]): Plan[K, F, O, A] = new Plan[K, F, O, A] {
    def apply[R](sym: PlanS[K, F, O, A, R]): R = sym.effect(eff, sym.done)
  }

  def exhaust[K[_], F[_], O](f: F[Option[O]])(implicit F: Monad[F]): Plan[K, F, O, Unit] =
    lift[K, F, O, Option[O]](f)
      .flatMap(emitOption)
      .flatMap(_ => exhaust(f))
}
