package paermar
package application

import paermar.model._, Domain._
import paermar.utility._
import scala.slick.jdbc.JdbcBackend
import org.joda.time.DateTime
import paermar.model.Domain.PersistenceModule.UnifiedPersistence
import paermar.utility.UnitOfWork.UnitOfWork
import paermar.utility.UnitOfWork

trait FeatureUniverse {
  import Parcel._, Domain.PersistenceModule._
  import scala.concurrent.{ Future, ExecutionContext }

  val persistence: UnifiedPersistence
  val database:    JdbcBackend#Database

  def databaseMap[X, A](f: JdbcBackend#Session ⇒ X ⊕ A): X ⊕ A = database withSession f
  def asyncDatabaseMap[X, A](f: JdbcBackend#Session ⇒ X ⊕ A)
                            (implicit ec: ExecutionContext): Future[X ⊕ A] =
    Future(databaseMap(f))

  def requiredParcelFromOption[A](item: Option[A], failMessage: ⇒ String): String ⊕ A = item match {
    case Some(data) ⇒ successful(data)
    case _          ⇒ failed(failMessage)
  }

  def run[R](work: UnitOfWork[R]) = (UnitOfWork evaluate work)(database)
}

trait AuthenticationFeatures { self: FeatureUniverse ⇒
  import Parcel._
  import persistence.profile.simple._

  case class AuthenticationContext(fullName: String)

  def authenticate(login: String, password: String) = UnitOfWork { implicit session ⇒
    parcelled(for (name ← persistence.findAuthenticatedAccount(login, password).firstOption)
    yield AuthenticationContext(name))
  }
}

trait CustomerFeatures { self: FeatureUniverse ⇒
  import persistence.profile.simple._
  import Parcel._
  import CustomersModule._

  def customer(id: Int) = UnitOfWork { implicit session ⇒
    parcelled(persistence findCustomerById id)
  }

  def customers = UnitOfWork { implicit session ⇒
    println(s"CustomerFeatures.customers")

    persistence.customers.buildColl[Seq].asSuccessful
  }

  def customerQuery(term: String) = UnitOfWork { implicit session ⇒
    println(s"CustomerFeatures.customerQuery query: $term")

    persistence.findCustomerByNameQuery(term).buildColl[Seq].asSuccessful
  }

  def addCustomer(name: String) = UnitOfWork { implicit session ⇒
    customer(persistence insertCustomer name)(session)
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

  def transactionsSpanning(from: DateTime, through: DateTime) = UnitOfWork { implicit session ⇒
    // There's no error handling anywhere here.

    // Sbould `successful` turn into a `failed` on exceptions?
    persistence.transactionsSpanning(from, through).buildColl[Seq].asSuccessful
  }

  def addTransaction(tx: Transaction) = UnitOfWork { implicit session ⇒
    val txId1 = tx match {
      case ValidDebit(customerId, amount, comment) ⇒
        parcelled (
          for (customer ← persistence findCustomerById customerId)
            yield persistence insertDebit (customer, amount, comment)
        ) orFailWith "transaction.no_such_customer"

      case ValidCredit(customerId, amount, comment) ⇒
        parcelled (
          for (customer ← persistence findCustomerById customerId)
          yield persistence insertCredit (customer, amount, comment)
        ) orFailWith "transaction.no_such_customer"

      case tx0 ⇒
        failed(s"transaction.not_valid: $tx0")
    }

    txId1 flatMapOption persistence.findTransactionById
  }

  def transaction(id: Int) = UnitOfWork { implicit session ⇒
    parcelled(persistence findTransactionById id)
  }
}

trait DepositFeatures { self: FeatureUniverse with TransactionFeatures ⇒
  import persistence.profile.simple._
  import Parcel._
  import DepositsModule._, TransactionsModule._, CustomersModule._

  def depositsSpanning(from: DateTime, through: DateTime) = UnitOfWork { implicit session ⇒
    persistence.depositsSpanning(from, through).buildColl[Seq].asSuccessful
  }

  def addDeposit(payment: Verification) = UnitOfWork { implicit session ⇒
    payment match {
      case CashVerification(customerId, amount, reference) ⇒
        val deposit = for {
          customer     ← persistence findCustomerById customerId

          transactionId = persistence.insertCredit(customer, amount, Option("deposit.cash"))
          transaction  ← persistence findTransactionById transactionId

          depositId     = persistence.insertDeposit(transaction.created, amount, reference, customer.name, None, Option(transactionId), Option("deposit.cash"))
          deposit      ← persistence findDepositById depositId
        } yield deposit

        parcelled(deposit)

      case BankGiroVerification(valueDate, amount, reference, payer, avi) ⇒
        val id      = persistence.insertDeposit(valueDate, amount, reference, payer, Option(avi), None, None)
        val deposit = persistence findDepositById id

        parcelled(deposit)
    }
  }

  def deposit(id: Int) = UnitOfWork { implicit session ⇒
    parcelled(persistence findDepositById id)
  }

  def creditDepositToCustomer(credit: CreditCustomerDeposit) = UnitOfWork { implicit session ⇒

    // What error conditions are there here?
    // Can there ever be a problem with a deposit being claimed first by
    // customer A and later by customer B? What happens to their net dept?

    parcelled(for {
      deposit       ← persistence findDepositById credit.depositId
      customer      ← persistence findCustomerById credit.customerId
      transactionId = persistence.insertCredit(customer, deposit.amount, credit.comment)
      transaction   ← persistence findTransactionById transactionId
      _             = persistence.setDepositTransaction(deposit, transactionId)

    } yield transaction)
  }
}

trait ProductFeatures { self: FeatureUniverse ⇒
  import persistence.profile.simple._
  import Parcel._
  import ProductsModule._

  def products = UnitOfWork { implicit session ⇒
    persistence.products.buildColl[Seq].asSuccessful
  }

  def addProduct(product: Product) = UnitOfWork { implicit session ⇒
    parcelled(product match {
      case Product(_, typ, name, unitPrice, description) ⇒
        val id = persistence insertProduct(typ, name, unitPrice, description)

        persistence findProductById id
    })
  }
}

trait SubscriptionFeatures { self: FeatureUniverse ⇒
  import persistence.profile.simple._
  import Parcel._
  import SubscriptionsModule._

  // Date span here too?
  def subscriptions = UnitOfWork { implicit session ⇒
    persistence.subscriptions.buildColl[Seq].asSuccessful
  }

  def addSubscription(subscription: Subscription) = UnitOfWork { implicit session ⇒
    parcelled(subscription match {
      case Subscription(_, customerId, _, startDate, comment) ⇒
        for {
            customer       ← persistence findCustomerById customerId
            subscriptionId = persistence.insertSubscription(customer, startDate, comment)
            subscription   ← persistence findSubscriptionById subscriptionId
        } yield subscription
    })
  }
}

trait OrderFeatures { self: FeatureUniverse ⇒
  import persistence.profile.simple._
  import paermar.utility._, UnitOfWork._
  import Parcel._
  import OrdersModule._, CustomersModule._

  def orders = UnitOfWork { implicit session ⇒
    persistence.orders.buildColl[Seq].asSuccessful
  }

  def ordersWithCustomers = UnitOfWork { implicit session ⇒
    persistence.orderCustomerJoin.buildColl[Seq].asSuccessful
  }

  def addOrder(order: OrderInsert) = UnitOfWork { implicit session ⇒
    parcelled(order match {
      case NewOrder(customerId, comment) ⇒
        for {
          customer ← persistence findCustomerById customerId
          orderId  = persistence.insertOrder(customer, comment)
          order    ← persistence findOrderById orderId
        } yield order
    })
  }

  def order(id: Int) = UnitOfWork[Order] { implicit session ⇒
    parcelled(persistence findOrderById id)
  }
}

trait TaskFeatures { self: FeatureUniverse with CustomerFeatures ⇒
  import persistence.profile.simple._
  import Parcel._
  import TasksModule._, TaskStatuses._

  def tasks = UnitOfWork { implicit session ⇒
    persistence.tasks.buildColl[Seq].asSuccessful
  }

  def addTask(task: TaskMemento) = UnitOfWork { implicit session ⇒
    parcelled(task match {
      case PlainTask(name, customerId, dueDate) ⇒
        val customer = customerId flatMap persistence.findCustomerById
        val taskId   = persistence.insertTask(name, customer, dueDate, None, None)

        persistence findTaskById taskId

      case ScheduledTask(name, customerId, deadline, scheduleId) ⇒
        val customer = customerId flatMap persistence.findCustomerById
        val schedule = persistence findScheduleById scheduleId
        val taskId   = persistence.insertTask(name, customer, Option(deadline), schedule, None)

        persistence findTaskById taskId

      case TaskSnapshot(name, status, deadline, scheduleId, comment, customerId) ⇒
        val customer = customerId flatMap persistence.findCustomerById
        val schedule = scheduleId flatMap persistence.findScheduleById
        val taskId   = persistence.insertTask(name, customer, Option(deadline), schedule, comment)

        persistence findTaskById taskId
    })
  }

  def task(id: Int) = UnitOfWork { implicit session ⇒
    parcelled(persistence findTaskById id)
  }

  def mergeTask(id: Int, task: TaskMemento) = UnitOfWork { implicit session ⇒
    parcelled(task match {
      case PlainTask(name, customerId, dueDate) ⇒
        val customer = customerId flatMap persistence.findCustomerById

/*
        persistence.mergeTask(id)(
          name     = Option(name),
          customer = customer,
          dueDate  = dueDate)
*/

        persistence findTaskById id

      case ScheduledTask(name, customerId, deadline, scheduleId) ⇒
        val customer = customerId flatMap persistence.findCustomerById
        val schedule = persistence findScheduleById scheduleId
        val taskId   = persistence.insertTask(name, customer, Option(deadline), schedule, None)

        persistence findTaskById taskId
    })
  }

  def replaceTask(id: Int, task: TaskMemento) = UnitOfWork { implicit session ⇒
    parcelled(task match {
      case PlainTask(name, customerId, dueDate) ⇒
        val customer = customerId flatMap (persistence findCustomerById _)

        // add another TaskMemento implementation that
        // provides the works.
/*
        persistence.replaceTask(id)(
          name     = name,
          customer = customer,
          dueDate  = dueDate)
*/

        persistence findTaskById id

      case ScheduledTask(name, customerId, deadline, scheduleId) ⇒
        val customer = customerId flatMap (persistence findCustomerById _)
        val schedule = persistence findScheduleById scheduleId
        val taskId   = persistence.insertTask(name, customer, Option(deadline), schedule, None)

        persistence findTaskById taskId
    })
  }
}

trait ShipmentFeatures { self: FeatureUniverse with OrderFeatures ⇒
  import persistence.profile.simple._
  import paermar.utility._
  import Parcel._
  import ShipmentsModule._

  def shipments = UnitOfWork { implicit session ⇒
    persistence.shipments.buildColl[Seq].asSuccessful
  }

  def shipment(id: Int) = UnitOfWork { implicit session ⇒
    parcelled(persistence findShipmentById id)
  }

  def addShipment(orderId: Int) = for {
    order    ← order(orderId)
    shipment ← UnitOfWork { implicit s ⇒
      order map persistence.insertShipment flatMap (shipment(_)(s))
    }
  } yield shipment
}

// I should really combine application with model as there's no real
// gain at the moment.

class ApplicationFeatures(val persistence: UnifiedPersistence,
                          val database: JdbcBackend#Database) extends FeatureUniverse
  with AuthenticationFeatures
  with CustomerFeatures
  with TransactionFeatures
  with DepositFeatures
  with ProductFeatures
  with SubscriptionFeatures
  with OrderFeatures
  with TaskFeatures
  with ShipmentFeatures