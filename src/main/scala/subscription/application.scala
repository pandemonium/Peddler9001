package paermar
package application

import paermar.model.Domain
import paermar.model.Domain.UnifiedPersistence
import java.util.Date
import scala.slick.jdbc.JdbcBackend
import org.joda.time.DateTime

object Parcel {
  type ⊕ [A, B] = Parcel[A, B]

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

object Reporting {
  sealed trait Level
  case object Info extends Level
  case object Warning extends Level
  case object Error extends Level

  sealed trait Report {

  }
}

trait FeatureUniverse {
  import Parcel._
  type UP = UnifiedPersistence

  val persistence: UnifiedPersistence
  val database:    JdbcBackend#Database

  def parcelMap[X, A](f: JdbcBackend#Session ⇒ X ⊕ A): X ⊕ A = database withSession f
}

trait AuthenticationFeatures { self: FeatureUniverse ⇒
  import Parcel._
  import persistence.profile.simple._

  case class AuthenticationContext(fullName: String)

  def authenticate(login: String, password: String): String ⊕ AuthenticationContext = parcelMap { implicit session ⇒
      val authenticAccount = for (name <- persistence.findAuthenticatedAccount(login, password).firstOption)
        yield successful(AuthenticationContext(name))

      authenticAccount getOrElse failed("authentication.bad_username_or_password")
    }
}

trait CustomerFeatures { self: FeatureUniverse ⇒
  import persistence.profile.simple._
  import Parcel._

  def customers: String ⊕ Seq[UP#Customer] = parcelMap { implicit session ⇒
    successful(persistence.customers.buildColl)
  }

  def addCustomer(name: String): String ⊕ UP#Customer = parcelMap { implicit session ⇒
    val id       = persistence insertCustomer name
    val customer = persistence findCustomerById id firstOption

    customer map successful getOrElse failed("customer.created_yet_not_found")
  }
}

trait TransactionFeatures { self: FeatureUniverse with CustomerFeatures ⇒
  import persistence.profile.simple._
  import persistence.TransactionType._
  import Parcel._

  def transactions(from: DateTime, through: DateTime): String ⊕ Seq[UP#Transaction] = parcelMap { implicit session ⇒
    // There's no error handling anywhere here.

    // Sbould `successful` turn into a `failed` on exceptions?
    successful(persistence.transactionsSpanning(from, through).buildColl)
  }

  def addTransaction(tx: UP#Transaction): String ⊕ UP#Transaction = parcelMap { implicit session ⇒
    val txId1 = tx match {
      case persistence.Transaction(_, cid, _, Debit,  amount, comment) if amount > 0 ⇒
        val txId = for {
          customer <- persistence findCustomerById cid firstOption
        } yield persistence.insertDebit(customer, amount, comment)

        txId map successful getOrElse failed("transaction.no_such_customer")

      case persistence.Transaction(_, cid, _, Credit, amount, comment) if amount > 0 ⇒
        val txId = for {
          customer <- persistence findCustomerById cid firstOption
        } yield persistence.insertCredit(customer, amount, comment)

        txId map successful getOrElse failed("transaction.no_such_customer")

      case tx0 if tx0.amount <= 0 ⇒
        failed("transaction.bad_amount")
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
