import cats.Monad
import machines.Machine._

import scala.collection.generic.CanBuild

package object machines {
  type Process[+F[_], -I, +O] = Machine[F, I Is ?, O]

  implicit class MachineOps[F[_], K[_], O](private val self: Machine[F, K, O]) extends AnyVal {
    final def run_(implicit F: Monad[F]): F[Unit] = self match {
      case Stop => F.unit
      case Emit(_, t) => t.run_
      case a: Await[F, K, O] => a.stop.run_
      case e: Effect[F, K, O] => F.flatMap(e.effect)(z => e(z).run_)
      case s: Shift[F, K, O] => s().run_
    }

    final def foldLeft[A](acc: A)(f: (A, O) => A)(implicit F: Monad[F]): F[A] = self match {
      case Stop => F.pure(acc)
      case Emit(h, t) => t.foldLeft(f(acc, h))(f)
      case a: Await[F, K, O] => a.stop.foldLeft(acc)(f)
      case e: Effect[F, K, O] => F.flatMap(e.effect)(z => e(z).foldLeft(acc)(f))
      case s: Shift[F, K, O] => s().foldLeft(acc)(f)
    }

    final def foldLeftM[A](acc: A)(f: (A, O) => F[A])(implicit F: Monad[F]): F[A] = self match {
      case Stop => F.pure(acc)
      case Emit(h, t) => F.flatMap(f(acc, h))(a => t.foldLeftM(a)(f))
      case a: Await[F, K, O] => a.stop.foldLeftM(acc)(f)
      case e: Effect[F, K, O] => F.flatMap(e.effect)(z => e(z).foldLeftM(acc)(f))
      case s: Shift[F, K, O] => s().foldLeftM(acc)(f)
    }

    final def to[T[_]](implicit C: CanBuild[O, T[O]], F: Monad[F]): F[T[O]] =
      F.map(foldLeft[List[O]](Nil)((a, o) => o :: a)) { l =>
        val b = C()
        l.reverseIterator.foreach(b += _)
        b.result()
      }

    final def toList(implicit F: Monad[F]): F[List[O]] = to[List]

    final def toVector(implicit F: Monad[F]): F[Vector[O]] = to[Vector]

    final def through[U](p: Process[F, O, U]): Machine[F, K, U] = {
      def compose[A, B](m: Machine[F, K, A], p: Process[F, A, B]): Machine[F, K, B] = p match {
        case Stop => Stop
        case Emit(h, t) => Emit(h, compose(m, t))
        case awaitP: Await[F, A Is ?, B] => m match {
          case Stop => compose(Stop, awaitP.stop)
          case Emit(h, t) => compose(t, awaitP(awaitP.await(h)))
          case awaitM: Await[F, K, A] =>
            Await(awaitM.await, (a: awaitM.Z) => compose(awaitM(a), p), compose(awaitM.stop, p))
          case eff: Effect[F, K, A] => Effect(eff.effect, (e: eff.Z) => compose(eff(e), p))
          case shift: Shift[F, K, A] => Shift(compose(shift(), p))
        }
        case eff: Effect[F, A Is ?, B] => Effect(eff.effect, (e: eff.Z) => compose(m, eff(e)))
        case shift: Shift[F, A Is ?, B] => Shift(compose(m, shift()))
      }

      compose(self, p)
    }
  }
}
