package paermar
package utility

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A ⇒ B): F[B]
}

object MonadInstances {
  import Parcel._

  implicit val _OptionMonad: Monad[Option] = new Monad[Option] {
    def pure[A](a: ⇒ A)                                = Option(a)
    def flatMap[A, B](ma: Option[A])(f: A ⇒ Option[B]) = ma flatMap f
  }

  implicit def _ParcelInstance[X] = new Monad[({ type λ[α] = X ⊕ α })#λ] {
    def pure[A](a: ⇒ A) = successful[X, A](a)
    def flatMap[A, B](ma: X ⊕ A)(f: A ⇒ X ⊕ B) = ma flatMap f
  }
}

object Monad {
  implicit class MonadOps[A, M[_]: Monad](ma: M[A]) {
    def flatMap[B](f: A ⇒ M[B]): M[B] = (M flatMap ma)(f)
  }

  def M[M[_]: Monad] = implicitly[Monad[M]]
}

trait Monad[M[_]] {
  def pure[A](a: ⇒ A): M[A]
  def flatMap[A, B](ma: M[A])(f: A ⇒ M[B]): M[B]
}

trait Reader[I, A] {
  def map[B](f: A ⇒ B): Reader[I, B]
  def flatMap[B](f: A ⇒ Reader[I, B]): Reader[I, B]
  def apply(environment: I): A
}

object Reader {
  case class Rdr[I, A](action: I ⇒ A) extends Reader[I, A] {
    def map[B](f: A ⇒ B): Reader[I, B]                = copy(action andThen f)
    def flatMap[B](f: A ⇒ Reader[I, B]): Reader[I, B] = copy(e ⇒ f(apply(e))(e))
    def apply(environment: I): A = action(environment)
  }

  def apply[I, A](f: I ⇒ A): Reader[I, A] = Rdr(f)
}

object UnitOfWork {
  import scala.slick.jdbc._
  import Parcel._

  type UnitOfWork[R] = Reader[JdbcBackend#Session, String ⊕ R]

  def apply[R](action: JdbcBackend#Session ⇒ String ⊕ R): UnitOfWork[R] = Reader(action)

  def evaluate[R](doWork: UnitOfWork[R])(implicit database: JdbcBackend#Database) =
    database withSession { session ⇒ doWork(session) }
}

object Parcel {
  type ⊕ [A, B] = Parcel[A, B]

  implicit class ParcelOps[A](value: ⇒ A) {
    def asSuccessful[X, AA >: A] = Parcel successful value
    def asFailed                 = Parcel failed value
  }

  sealed trait Parcel[+X, +A] { self ⇒
    def isSuccessful: Boolean
    def isFailure: Boolean
    def isEmpty: Boolean

    def flatMap[X1 >: X, B](f: A ⇒ Parcel[X1, B]): Parcel[X1, B] = self match {
      case Successful(a) ⇒ f(a)
      case _             ⇒ self.asInstanceOf[Parcel[X, B]]
    }

    def map[B](f: A ⇒ B): Parcel[X, B] =
      if (isSuccessful) flatMap(a ⇒ successful(f(a)))
      else self.asInstanceOf[Parcel[X, B]]

    def flatMapOption[B](f: A ⇒ Option[B]): Parcel[X, B] =
      if (isSuccessful) flatMap(a ⇒ f(a).fold(empty[X, B])(successful))
      else self.asInstanceOf[Parcel[X, B]]

    def mapOrElse[B](f: A ⇒ B)(b: ⇒ B): B = self match {
      case Successful(a) ⇒ f(a)
      case _             ⇒ b
    }

    def orFailWith[XX >: X](x: ⇒ XX): Parcel[XX, A] =
      if (isEmpty) failed(x) else self

    // You cannot fold an empty parcel - what happens if you do?
    def fold[Z](f: X ⇒ Z, s: A ⇒ Z, empty: ⇒ Z): Z = self match {
      case Successful(a) ⇒ s(a)
      case Failed(x)     ⇒ f(x)
      case Empty         ⇒ empty
    }
  }

  // def deferred[X, A](a: ⇒ A): Future[Parcel[X, A]]
  def empty[X, A]: Parcel[X, A]                       = Empty
  def successful[X, A](a: A): Parcel[X, A]            = Successful[X, A](a)
  def failed[X, A](x: X): Parcel[X, A]                = Failed[X, A](x)
  def parcelled[X, A](item: Option[A]): Parcel[X, A] = item.fold(empty[X, A])(successful)

  private case object Empty extends Parcel[Nothing, Nothing] {
    val isEmpty      = true
    val isFailure    = false
    val isSuccessful = false
  }

  private case class Successful[X, A](a: A) extends Parcel[X, A] {
    val isEmpty      = false
    val isFailure    = false
    val isSuccessful = true
  }

  private case class Failed[X, A](x: X) extends Parcel[X, A] {
    val isEmpty      = false
    val isFailure    = true
    val isSuccessful = false
  }

  import Monad.M
  def sequence[X, A, M[_] : Monad](parcel: X ⊕ M[A]): M[X ⊕ A] = parcel.fold(
    x  ⇒ M pure failed[X, A](x),
    ma ⇒ (M flatMap ma)(a ⇒ M pure successful[X, A](a)),
    M pure empty[X, A]
  )

  def flatten[X, A](pp: Parcel[X, Parcel[X, A]]): Parcel[X, A] =
    pp.flatMap(_.fold(failed, successful, empty))
}