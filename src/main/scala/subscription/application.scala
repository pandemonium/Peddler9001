package paermar
package application

import paermar.model._, Domain._
import scala.slick.jdbc.JdbcBackend
import org.joda.time.DateTime

object Parcel {
  type ⊕ [A, B] = Parcel[A, B]

  implicit class ParcelOps[A](value: A) {
    def successfulParcel[X, AA >: A] = Parcel successful value
    def failedParcel                 = Parcel failed value
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
  import Parcel._, Domain.PersistenceModule._
  import scala.concurrent.{ Future, ExecutionContext }

  val persistence: UnifiedPersistence
  val database:    JdbcBackend#Database

  def databaseMap[X, A](f: JdbcBackend#Session ⇒ X ⊕ A): X ⊕ A = database withSession f
  def asyncDatabaseMap[X, A](f: JdbcBackend#Session ⇒ X ⊕ A)
                            (implicit ec: ExecutionContext): Future[X ⊕ A] =
    Future(databaseMap(f))

  def parcelFromOption[A](item: Option[A], failMessage: ⇒ String): String ⊕ A = item match {
    case Some(data) ⇒ successful(data)
    case _       ⇒ failed(failMessage)
  }
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
  import CustomersModule._

  def customers: String ⊕ Seq[Customer] = databaseMap { implicit session ⇒
    persistence.customers.buildColl.successfulParcel
  }

  def addCustomer(name: String): String ⊕ Customer = databaseMap { implicit session ⇒
    val id       = persistence insertCustomer name
    val customer = persistence findCustomerById id firstOption

    customer map successful getOrElse "customer.created_yet_not_found".failedParcel
  }
}

trait TransactionFeatures { self: FeatureUniverse with CustomerFeatures ⇒
  import persistence.profile.simple._
  import Parcel._
  import TransactionsModule._, TransactionType._

  class ValidatingTransactionExtractor(val `type`: TransactionType) {
    def unapply(tx: Transaction) =
      if (tx.amount > 0 && tx.`type` == `type`) Some((tx.customerId, tx.amount, tx.comment))
      else None
  }
  val ValidDebit  = new ValidatingTransactionExtractor(Debit)
  val ValidCredit = new ValidatingTransactionExtractor(Credit)

  def transactionsSpanning(from: DateTime, through: DateTime): String ⊕ Seq[Transaction] = databaseMap { implicit session ⇒
    // There's no error handling anywhere here.

    // Sbould `successful` turn into a `failed` on exceptions?
    persistence.transactionsSpanning(from, through).buildColl.successfulParcel
  }

  def addTransaction(tx: Transaction): String ⊕ Transaction = databaseMap { implicit session ⇒
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
      case _       ⇒ failed("transaction.created_yet_not_found")
    }
  }
}

trait DepositFeatures { self: FeatureUniverse with TransactionFeatures ⇒
  import persistence.profile.simple._
  import Parcel._
  import DepositsModule._, TransactionsModule._

  def depositsSpanning(from: DateTime, through: DateTime): String ⊕ Seq[Deposit] = databaseMap { implicit session ⇒
    persistence.depositsSpanning(from, through).buildColl.successfulParcel
  }

  def addDeposit(payment: Payment): String ⊕ Deposit = databaseMap { implicit session ⇒
    payment match {
      case CashPayment(customerId, amount, reference) ⇒
        val deposit = for {
          customer     <- persistence findCustomerById customerId firstOption

          transactionId = persistence.insertCredit(customer, amount, Option("deposit.cash"))
          transaction  <- persistence findTransactionById transactionId firstOption

          depositId     = persistence.insertDeposit(transaction.created, amount, reference, customer.name, None, Option(transactionId), Option("deposit.cash"))
          deposit      <- persistence findDepositById depositId firstOption
        } yield deposit

        parcelFromOption(deposit, "deposit.created_yet_not_found")

      case BankGiroVerification(valueDate, amount, reference, payer, avi) ⇒
        val id      = persistence.insertDeposit(valueDate, amount, reference, payer, Option(avi), None, None)
        val deposit = persistence findDepositById id firstOption

        parcelFromOption(deposit, "deposit.created_yet_not_found")
    }
  }
}

trait ProductFeatures { self: FeatureUniverse ⇒
  import persistence.profile.simple._
  import Parcel._
  import ProductsModule._

  def products: String ⊕ Seq[Product] = databaseMap { implicit session ⇒
    persistence.products.buildColl.successfulParcel
  }

  def addProduct(product: Product): String ⊕ Product = databaseMap { implicit session ⇒
    parcelFromOption(product match {
      case Product(_, typ, name, unitPrice, description) ⇒
        val id = persistence.insertProduct(typ, name, unitPrice, description)

        persistence findProductById id firstOption
    }, "product.created_yet_not_found")
  }
}

trait SubscriptionFeatures { self: FeatureUniverse ⇒
  import persistence.profile.simple._
  import Parcel._
  import SubscriptionsModule._

  // Date span here too?
  def subscriptions: String ⊕ Seq[Subscription] = databaseMap { implicit session ⇒
    persistence.subscriptions.buildColl.successfulParcel
  }

  def addSubscription(subscription: Subscription): String ⊕ Subscription = databaseMap { implicit session ⇒
    parcelFromOption(subscription match {
      case Subscription(_, customerId, _, startDate, comment) ⇒
        for {
            customer      <- persistence findCustomerById customerId firstOption

            subscriptionId = persistence.insertSubscription(customer, startDate, comment)
            subscription  <- persistence findSubscriptionById subscriptionId firstOption
        } yield subscription
    }, "subscription.created_yet_not_found")
  }
}

trait OrderFeatures { self: FeatureUniverse ⇒
  import persistence.profile.simple._
  import Parcel._
  import OrdersModule._

  def orders: String ⊕ Seq[Order] = databaseMap { implicit session ⇒
    persistence.orders.buildColl.successfulParcel
  }

  def addOrder(order: OrderInsert): String ⊕ Order = databaseMap { implicit session ⇒
    parcelFromOption(order match {
      case NewOrder(customerId, comment) ⇒
        for {
          customer <- persistence findCustomerById customerId firstOption

          orderId   = persistence.insertOrder(customer, comment)
          order    <- persistence findOrderById orderId firstOption
        } yield order
    }, "orders.created_yet_not_found")
  }

  def order(id: Int): String ⊕ Option[Order] = databaseMap { implicit session ⇒
    val order = persistence findOrderById id firstOption

    // Still no real error handling. An exception bubbles all the way
    // up as an Internal Server Error.

    order.successfulParcel
  }
}

class ApplicationFeatures(val persistence: Domain.PersistenceModule.UnifiedPersistence,
                          val database: JdbcBackend#Database) extends FeatureUniverse
  with AuthenticationFeatures
  with CustomerFeatures
  with TransactionFeatures
  with DepositFeatures
  with ProductFeatures
  with SubscriptionFeatures
  with OrderFeatures