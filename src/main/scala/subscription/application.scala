package paermar
package application

import paermar.model.Domain.UnifiedPersistence
import scala.slick.jdbc.JdbcBackend
import org.joda.time.DateTime

object Parcel {
  type ⊕ [A, B] = Parcel[A, B]

  implicit class ParcelOps[A](value: A) {
    def successfulParcel[X, AA <: A] = Parcel successful value
    def failedParcel     = Parcel failed value
  }

  sealed trait Parcel[+X, +A] { self ⇒
    def isSuccessful: Boolean
    def isFailure: Boolean

    def flatMap[X1 >: X, B](f: A ⇒ Parcel[X1, B]): Parcel[X1, B] = self match {
      case Successful(a) ⇒ f(a)
      case _             ⇒ self.asInstanceOf[Parcel[X, B]]
    }

    def map[B](f: A ⇒ B): Parcel[X, B] =
      if (isSuccessful) flatMap(a ⇒ successful(f(a)))
      else self.asInstanceOf[Parcel[X, B]]

    def mapOrElse[B](f: A ⇒ B)(b: ⇒ B): B = self match {
      case Successful(a) ⇒ f(a)
      case _             ⇒ b
    }

    def fold[Z](f: X ⇒ Z, s: A ⇒ Z): Z = self match {
      case Successful(a) ⇒ s(a)
      case Failed(x)     ⇒ f(x)
    }
  }

  // def deferred[X, A](a: ⇒ A): Future[Parcel[X, A]]
  def successful[X, A](a: A): Parcel[X, A] = Successful[X, A](a)
  def failed[X, A](x: X): Parcel[X, A]     = Failed[X, A](x)

  private case class Successful[X, A](a: A) extends Parcel[X, A] {
    def isFailure    = false
    def isSuccessful = true
  }

  private case class Failed[X, A](x: X) extends Parcel[X, A] {
    def isFailure    = true
    def isSuccessful = false
  }
}

trait FeatureUniverse {
  import Parcel._
  type UP = UnifiedPersistence

  val persistence: UnifiedPersistence
  val database:    JdbcBackend#Database

  def databaseMap[X, A](f: JdbcBackend#Session ⇒ X ⊕ A): X ⊕ A = database withSession f
}

trait AuthenticationFeatures { self: FeatureUniverse ⇒
  import Parcel._
  import persistence.profile.simple._

  case class AuthenticationContext(fullName: String)

  def authenticate(login: String, password: String): String ⊕ AuthenticationContext = databaseMap { implicit session ⇒
      val authenticAccount = for (name <- persistence.findAuthenticatedAccount(login, password).firstOption)
        yield successful(AuthenticationContext(name))

      authenticAccount getOrElse failed("authentication.bad_username_or_password")
    }
}

trait CustomerFeatures { self: FeatureUniverse ⇒
  import persistence.profile.simple._
  import Parcel._

  def customers: String ⊕ Seq[UP#Customer] = databaseMap { implicit session ⇒
    persistence.customers.buildColl.successfulParcel
  }

  def addCustomer(name: String): String ⊕ UP#Customer = databaseMap { implicit session ⇒
    val id       = persistence insertCustomer name
    val customer = persistence findCustomerById id firstOption

    customer map successful getOrElse "customer.created_yet_not_found".failedParcel
  }
}

trait TransactionFeatures { self: FeatureUniverse with CustomerFeatures ⇒
  import persistence.profile.simple._
  import persistence.TransactionType._
  import Parcel._

  class ValidatingTransactionExtractor(val `type`: TransactionType) {
    def unapply(x: UP#Transaction) =
      if (x.amount > 0 && x.`type` == `type`) Some((x.customerId, x.amount, x.comment))
      else None
  }
  val ValidDebit  = new ValidatingTransactionExtractor(Debit)
  val ValidCredit = new ValidatingTransactionExtractor(Credit)

  def transactions(from: DateTime, through: DateTime): String ⊕ Seq[UP#Transaction] = databaseMap { implicit session ⇒
    // There's no error handling anywhere here.

    // Sbould `successful` turn into a `failed` on exceptions?
    persistence.transactionsSpanning(from, through).buildColl.successfulParcel
  }

  def addTransaction(tx: UP#Transaction): String ⊕ UP#Transaction = databaseMap { implicit session ⇒
    val txId1 = tx match {
      case ValidDebit(customerId, amount, comment) ⇒
        val txId = for {
          customer <- persistence findCustomerById customerId firstOption
        } yield persistence.insertDebit(customer, amount, comment)

        txId map successful getOrElse failed("transaction.no_such_customer")

      case ValidCredit(customerId, amount, comment) ⇒
        val txId = for {
          customer <- persistence findCustomerById customerId firstOption
        } yield persistence.insertCredit(customer, amount, comment)

        txId map successful getOrElse failed("transaction.no_such_customer")

      case tx0 ⇒
        failed(s"transaction.not_valid: $tx0")
    }

    txId1 map (persistence findTransactionById _ firstOption) flatMap {
      case Some(x) ⇒ successful(x)
      case _    ⇒ failed("transaction.created_yet_not_found")
    }
  }
}

class ApplicationFeatures(val persistence: UnifiedPersistence,
                          val database: JdbcBackend#Database) extends FeatureUniverse
  with AuthenticationFeatures
  with CustomerFeatures
  with TransactionFeatures

/*

{
  import application.Parcel._


  // How does this whole thing deal with errors?
  // Make everything return Either[<Error type>, <Result type>]?


  def newCustomer(name: String, email: String): Parcel[String, persistence.Customer]

  // What if it fails - how does it report it?
  //   Either[String, persistence.Deposit]
  //   Validation?
  def newDeposit(valueDate: Date, amount: Int): Parcel[String, persistence.Deposit]
  def deposits(from: Date, through: Date): Parcel[String, Seq[persistence.Deposit]]
  def deposit(id: Int): Parcel[String, persistence.Deposit]
}
*/
