package paermar
package model

import slick.driver.JdbcProfile
import org.joda.time.DateTime
import com.github.tototoshi.slick.MySQLJodaSupport._
import scala.slick.lifted.CompiledFunction
import scala.slick.lifted

/*
  Add logging.
*/

trait PersistentUniverse {
  val profile: JdbcProfile

  def currentDateTime = new DateTime
}

trait Structure { self: PersistentUniverse ⇒
  def description: profile.DDL
}

abstract class Foundation extends PersistentUniverse with Structure {
  def description = profile.DDL(Iterable.empty, Iterable.empty)
}

object Domain {
  trait AbstractModule

  object CustomersModule extends AbstractModule {
    case class Customer(id: Option[Int], name: String, created: DateTime)

    trait CustomersAspect extends Structure { self: PersistentUniverse ⇒
      import profile.simple._

      class Customers(tag: Tag) extends Table[Customer](tag, "Customers") {
        def id      = column[Int]("id",      O.PrimaryKey, O.AutoInc)
        def name    = column[String]("name", O.NotNull)
        def created = column[DateTime]("created", O.NotNull)

        def *       = (id ?, name, created) <> (Customer.tupled, Customer.unapply)
      }

      val customers = TableQuery[Customers]

      override abstract def description = customers.ddl ++ super.description

      def insertCustomer(name: String)(implicit s: Session): Int =
        customers returning customers.map(_.id) += Customer(None, name, currentDateTime)

      def findCustomerById(id: Int)(implicit session: Session) =
        customers.findBy(_.id).applied(id).firstOption

      def findCustomerByNameQuery(term: String)
                                 (implicit session: Session) =
        for (
          c ← customers
            if c.name like s"%$term%"
        ) yield c
    }
  }

  object TransactionsModule extends AbstractModule {
    import CustomersModule._, TransactionType._


    /* This aspect really does not benefit from having a
       `bean` modelling its data.
     */

    object TransactionType {
      sealed abstract class TransactionType (val value: Int)
      case object Debit extends TransactionType(1)
      case object Credit extends TransactionType(2)

      def fromInt(value: Int) = value match {
        case 1 ⇒ Debit
        case 2 ⇒ Credit
      }
    }

    case class Transaction(id: Option[Int],
                           customerId: Int,
                           created: DateTime,
                           `type`: TransactionType,
                           amount: Int,
                           comment: Option[String])

    trait TransactionsAspect extends Structure { self: PersistentUniverse with CustomersAspect ⇒
      import profile.simple._
      class Transactions(tag: Tag) extends Table[Transaction](tag, "Transactions") {
        def id         = column[Int]("id",      O.PrimaryKey, O.AutoInc)
        def customerId = column[Int]("customerId", O.NotNull)
        def created    = column[DateTime]("created", O.NotNull)
        def `type`     = column[TransactionType]("type", O.NotNull)
        def amount     = column[Int]("amount", O.NotNull)
        def comment    = column[String]("comment")

        def *          = (id ?, customerId, created, `type`, amount, comment ?) <>
                           (Transaction.tupled, Transaction.unapply)

        def customer   = foreignKey("customerFk", customerId, customers)(_.id)

        implicit val transactionType =
          MappedColumnType.base[TransactionType, Int](_.value, TransactionType.fromInt)
      }

      val transactions = TableQuery[Transactions]

      override abstract def description = transactions.ddl ++ super.description

      // This is so going to blow up with some dumbass NullPointerException
      val transactionInserts = transactions returning transactions.map(_.id)

      def insertDebit(customer: Customer, amount: Int, comment: Option[String] = None)
                     (implicit s: Session): Int =
        transactionInserts += Transaction(None, customer.id.get, currentDateTime, Debit, amount, comment)

      def insertCredit(customer: Customer, amount: Int, comment: Option[String])
                      (implicit s: Session): Int =
        transactionInserts += Transaction(None, customer.id.get, currentDateTime, Credit, amount, comment)

      def transactionsSpanning(from: DateTime, through: DateTime) = for {
        tx ← transactions
        if tx.created >= from && tx.created <= through
      } yield tx

      def findTransactionById(id: Int)(implicit s: Session) =
        transactions.findBy(_.id).applied(id).firstOption
    }
  }

  object DepositsModule extends AbstractModule {
    import TransactionsModule._, CustomersModule._

    sealed trait Verification
    case class CashVerification(customerId: Int, amount: Int, reference: String)
      extends Verification
    case class BankGiroVerification(valueDate: DateTime,
                                    amount: Int,
                                    reference: String,
                                    payer: String,
                                    avi: String)
      extends Verification

    case class Deposit(id: Option[Int],
                       valueDate: DateTime,
                       created: DateTime,
                       amount: Int,
                       reference: String,
                       payer: String,
                       avi: Option[String],
                       transactionId: Option[Int],
                       comment: Option[String])

    case class CreditCustomerDeposit(customerId: Int,
                                     depositId: Int,
                                     comment: Option[String])

    trait DepositsAspect extends Structure {
      self: PersistentUniverse with TransactionsAspect with CustomersAspect ⇒
      import profile.simple._

      class Deposits(tag: Tag) extends Table[Deposit](tag, "Deposits") {
        def id            = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def valueDate     = column[DateTime]("valueDate", O.NotNull)
        def created       = column[DateTime]("created", O.NotNull)
        def amount        = column[Int]("amount", O.NotNull)
        def reference     = column[String]("reference", O.NotNull)
        def payer         = column[String]("payer", O.NotNull)
        def avi           = column[String]("avi", O.NotNull)
        def transactionId = column[Int]("transactionId")
        def comment       = column[String]("comment")

        def transaction   = foreignKey("transactionFk", transactionId, transactions)(_.id)

        def * = (id ?, valueDate, created, amount, reference, payer, avi ?, transactionId ?, comment ?) <>
                  (Deposit.tupled, Deposit.unapply)
      }

      val deposits       = TableQuery[Deposits]
      val depositInserts = deposits returning deposits.map(_.id)

      override abstract def description = deposits.ddl ++ super.description

      def insertDeposit(customer: Customer,
                        amount: Int,
                        reference: String)
                       (implicit s: Session): Int =
        insertDeposit(new DateTime, amount, reference, customer.name, None, None, None)

      def insertDeposit(valueDate: DateTime,
                        amount: Int,
                        reference: String,
                        payer: String,
                        avi: Option[String],
                        transactionId: Option[Int],
                        comment: Option[String])
                       (implicit s: Session): Int =
        depositInserts += Deposit(None, valueDate, currentDateTime, amount, reference, payer, avi, transactionId, comment)

      def setDepositTransaction(deposit: Deposit, transactionId: Int)
                               (implicit session: Session) = {
        val projection = for (d ← deposits if d.id === deposit.id.get)
                           yield d.transactionId

        projection update transactionId
      }

      def depositsSpanning(from: DateTime, through: DateTime) = for {
        d ← deposits
        if d.created >= from && d.created <= through
      } yield d

      def findDepositById(id: Int)(implicit s: Session) =
        deposits.findBy(_.id).applied(id).firstOption
    }
  }

  object ProductsModule extends AbstractModule {

    // What are product types? Programme | Subscription?
    // Are there more types than those two?

    case class Product(id: Option[Int],
                       `type`: Int,
                       name: String,
                       unitPrice: Int,
                       description: Option[String])

    trait ProductsAspect extends Structure { self: PersistentUniverse ⇒
      import profile.simple._

      class Products(tag: Tag) extends Table[Product](tag, "Products") {
        def id          = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def `type`      = column[Int]("type", O.NotNull)
        def name        = column[String]("name", O.NotNull)
        def unitPrice   = column[Int]("unitPrice", O.NotNull)
        def description = column[String]("description")

        def *           = (id ?, `type`, name, unitPrice, description ?) <>
                            (Product.tupled, Product.unapply)
      }


      val products       = TableQuery[Products]
      val productInserts = products returning products.map(_.id)

      override abstract def description = products.ddl ++ super.description

      def insertProduct(`type`: Int,
                        name: String,
                        unitPrice: Int,
                        description: Option[String])
                       (implicit s: Session) =
        productInserts += Product(None, `type`, name, unitPrice, description)

      def findProductById(id: Int)(implicit s: Session) =
        products.findBy(_.id).applied(id).firstOption
    }
  }

  object OrdersModule extends AbstractModule {
    import CustomersModule._, ProductsModule._, OrderStatus._

    object OrderStatus {
      sealed abstract class OrderStatus(val value: Int)
      case object New extends OrderStatus(1)

      def fromInt(value: Int) = value match {
        case 1 ⇒ New
      }
    }

    sealed trait OrderInsert
    case class NewOrder(customerId: Int, comment: Option[String])
      extends OrderInsert

    case class Order(id: Option[Int],
                     customerId: Int,
                     created: DateTime,
                     status: OrderStatus,
                     comment: Option[String])

    case class OrderLine(id: Option[Int],
                         orderId: Int,
                         productId: Int,
                         quantity: Int,
                         description: Option[String])

    trait OrdersAspect extends Structure {
      self: PersistentUniverse with CustomersAspect with ProductsAspect ⇒
      import profile.simple._

      class Orders(tag: Tag) extends Table[Order](tag, "Orders") {
        def id         = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def customerId = column[Int]("customerId", O.NotNull)
        def created    = column[DateTime]("created", O.NotNull)
        def status     = column[OrderStatus]("status", O.NotNull)
        def comment    = column[String]("comment")

        def customer   = foreignKey("customerFk", customerId, customers)(_.id)

        def *          = (id ?, customerId, created, status, comment ?) <>
          (Order.tupled, Order.unapply)

        implicit val orderStatus =
          MappedColumnType.base[OrderStatus, Int](_.value, OrderStatus.fromInt)
      }

      class OrderLines(tag: Tag) extends Table[OrderLine](tag, "OrderLines") {
        def id          = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def orderId     = column[Int]("orderId", O.NotNull)
        def productId   = column[Int]("productId", O.NotNull)
        def quantity    = column[Int]("quantity", O.NotNull)
        def description = column[String]("description")

        def order       = foreignKey("orderFk", orderId, orders)(_.id)
        def product     = foreignKey("productFk", productId, products)(_.id)

        def * = (id ?, orderId, productId, quantity, description ?) <>
          (OrderLine.tupled, OrderLine.unapply)
      }

      val orders           = TableQuery[Orders]
      val orderInserts     = orders returning orders.map(_.id)
      val orderLines       = TableQuery[OrderLines]
      val orderLineInserts = orderLines returning orderLines.map(_.id)

      override abstract def description = orders.ddl ++ orderLines.ddl ++ super.description

      // Why can't this thing utilise the foreign key thing?
      val orderCustomerJoin = for {
        (order, customer) ← orders innerJoin customers on (_.customerId === _.id)
      } yield (order, customer)

      def insertOrder(customer: Customer, comment: Option[String])
                     (implicit s: Session) =
        orderInserts += Order(None, customer.id.get, currentDateTime, New, comment)

      def insertOrderLine(order: Order,
                          product: Product,
                          quantity: Int,
                          description: Option[String])
                         (implicit s: Session) =
        orderLineInserts += OrderLine(None, order.id.get, product.id.get, quantity, description)

      def findOrderById(id: Int)(implicit s: Session) =
        orders.findBy(_.id).applied(id).firstOption

      def findOrderLineById(id: Int)(implicit s: Session) =
        orderLines.findBy(_.id).applied(id).firstOption
    }
  }

  object AccountsModule extends AbstractModule {
    case class Account(id: Option[Int],
                       login: String,
                       password: String,
                       fullName: String)

    trait AccountsAspect extends Structure { self: PersistentUniverse ⇒
      import profile.simple._

      class Accounts(tag: Tag) extends Table[Account](tag, "Accounts") {
        def id       = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def login    = column[String]("login", O.NotNull)
        def password = column[String]("password", O.NotNull)
        def fullName = column[String]("fullName", O.NotNull)

        def *        = (id ?, login, password, fullName) <> (Account.tupled, Account.unapply)
      }

      val accounts = TableQuery[Accounts]

      override abstract def description = accounts.ddl ++ super.description

      def insertAccount(login: String, password: String, fullName: String)
                       (implicit s: Session) =
        accounts += Account(None, login, password, fullName)

      def findAuthenticatedAccount(login: String, password: String)
                                  (implicit s: Session) =
        for (acct ← accounts
             if acct.login === login && acct.password === password)
        yield acct.fullName
    }
  }

  object SubscriptionsModule extends AbstractModule {
    import CustomersModule._

    case class Subscription(id: Option[Int],
                            customerId: Int,
                            created: DateTime,
                            startDate: Option[DateTime],
                            comment: Option[String])

    trait SubscriptionsAspect extends Structure { self: PersistentUniverse ⇒
      import profile.simple._

      class Subscriptions(tag: Tag) extends Table[Subscription](tag, "Subscriptions") {
        def id         = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def customerId = column[Int]("customerId", O.NotNull)
        def created    = column[DateTime]("created", O.NotNull)
        def startDate  = column[DateTime]("startDate")
        def comment    = column[String]("comment")

        def * = (id ?, customerId, created, startDate ?, comment ?) <>
                  (Subscription.tupled, Subscription.unapply)
      }

      val subscriptions      = TableQuery[Subscriptions]
      val subscriptionInsert = subscriptions returning subscriptions.map(_.id)

      override abstract def description = subscriptions.ddl ++ super.description

      def insertSubscription(customer: Customer, startDate: Option[DateTime], comment: Option[String])
                            (implicit s: Session) =
        subscriptionInsert += Subscription(None, customer.id.get, new DateTime, startDate, comment)

      def findSubscriptionById(id: Int)(implicit s: Session) =
        subscriptions.findBy(_.id).applied(id).firstOption
    }
  }

  object TasksModule extends AbstractModule {
    import CustomersModule._
    import TaskStatuses._, UnitTypes._

    sealed trait TaskMemento
    case class PlainTask(name: String,
                         customerId: Option[Int],
                         dueDate: Option[DateTime])
      extends TaskMemento
    case class ScheduledTask(name: String,
                             customerId: Option[Int],
                             deadline: DateTime,
                             scheduleId: Int)
      extends TaskMemento
    case class TaskSnapshot(name: String,
                            status: TaskStatus,
                            dueDate: DateTime,
                            scheduleId: Option[Int],
                            comment: Option[String],
                            customer: Option[Int])
      extends TaskMemento

    object UnitTypes {
      sealed abstract class UnitType private[UnitTypes] (val value: Int)
      case object Day   extends UnitType(1)
      case object Week  extends UnitType(2)
      case object Month extends UnitType(3)
      case object Year  extends UnitType(4)

      def fromInt(value: Int) = value match {
        case 1 ⇒ Day
        case 2 ⇒ Week
        case 3 ⇒ Month
        case 4 ⇒ Year
      }

      def unapply(`type`: UnitType) = `type` match {
        case Day   ⇒ Some(1)
        case Week  ⇒ Some(2)
        case Month ⇒ Some(3)
        case Year  ⇒ Some(4)
      }
    }

    case class Schedule(id: Option[Int],
                        name: String,
                        created: DateTime,
                        unitType: UnitType,
                        units: Int)

    object TaskStatuses {
      sealed abstract class TaskStatus private[TaskStatuses] (val value: Int)
      case object Open   extends TaskStatus(1)
      case object Closed extends TaskStatus(2)

      def fromInt(value: Int) = value match {
        case 1 ⇒ Open
        case 2 ⇒ Closed
      }
    }

    case class Task(id: Option[Int],
                    name: String,
                    created: DateTime,
                    status: TaskStatus,
                    lastUpdated: DateTime,
                    dueDate: Option[DateTime],
                    scheduleId: Option[Int],
                    comment: Option[String],
                    customerId: Option[Int])

    case class TaskActivity(id: Option[Int],
                            taskId: Int,
                            created: DateTime,
                            content: String)

    trait TasksAspect extends Structure { self: PersistentUniverse with CustomersAspect ⇒
      import profile.simple._

      class Schedules(tag: Tag) extends Table[Schedule](tag, "Schedules") {
        def id       = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def name     = column[String]("name", O.NotNull)
        def created  = column[DateTime]("created", O.NotNull)
        def unitType = column[UnitType]("unitType", O.NotNull)
        def units    = column[Int]("units", O.NotNull)

        def * = (id ?, name, created, unitType, units) <> (Schedule.tupled, Schedule.unapply)

        implicit val unitTypes =
          MappedColumnType.base[UnitType, Int](_.value, UnitTypes.fromInt)
      }

      class Tasks(tag: Tag) extends Table[Task](tag, "Task") {
        def id          = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def name        = column[String]("name", O.NotNull)
        def created     = column[DateTime]("created", O.NotNull)
        def status      = column[TaskStatus]("status", O.NotNull)
        def lastUpdated = column[DateTime]("lastUpdated", O.NotNull)
        def dueDate     = column[DateTime]("dueDate")
        def scheduleId  = column[Int]("scheduleId")
        def comment     = column[String]("comment")
        def customerId  = column[Int]("customerId")

        def customer    = foreignKey("customerFk", customerId, customers)(_.id)
        def schedule    = foreignKey("scheduleFk", scheduleId, schedules)(_.id)

        def * = (id ?, name, created, status, lastUpdated, dueDate ?, scheduleId ?, comment ?, customerId ?) <>
                  (Task.tupled, Task.unapply)

        implicit val taskStatus =
          MappedColumnType.base[TaskStatus, Int](_.value, TaskStatuses.fromInt)
      }

      class TaskActivities(tag: Tag) extends Table[TaskActivity](tag, "TaskActivities") {
        def id      = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def taskId  = column[Int]("taskId", O.NotNull)
        def created = column[DateTime]("created", O.NotNull)
        def content = column[String]("content", O.NotNull)

        def task    = foreignKey("taskFk", taskId, tasks)(_.id)

        def * = (id ?, taskId, created, content) <> (TaskActivity.tupled, TaskActivity.unapply)
      }

      val schedules           = TableQuery[Schedules]
      val scheduleInserts     = schedules returning schedules.map(_.id)
      val tasks               = TableQuery[Tasks]
      val taskInserts         = tasks returning tasks.map(_.id)
      val taskActivities      = TableQuery[TaskActivities]
      val taskActivityInserts = taskActivities returning taskActivities.map(_.id)

      override abstract def description =
        schedules.ddl ++ tasks.ddl ++ taskActivities.ddl ++ super.description


      def insertTask(name: String,
                     customer: Option[Customer],
                     due: Option[DateTime],
                     schedule: Option[Schedule],
                     comment: Option[String])
                    (implicit s: Session) =
        taskInserts += Task(None,
                            name,
                            new DateTime,
                            Open,
                            new DateTime,
                            due,
                            schedule.flatMap(_.id),
                            None,
                            customer.flatMap(_.id))

      def insertTaskComment(task: Task, comment: String)
                           (implicit s: Session) =
        taskActivityInserts += TaskActivity(None, task.id.get, new DateTime, comment)

      // replace entire state
      def replaceTask(id: Int)
                     (name: String,
                      status: TaskStatus,
                      customer: Option[Customer],
                      dueDate: Option[DateTime],
                      schedule: Option[Schedule],
                      comment: Option[String]) = ???

      def findTaskById(id: Int)(implicit s: Session) =
        tasks.findBy(_.id).applied(id).firstOption

      def findTaskActivityById(id: Int)(implicit s: Session) =
        taskActivities.findBy(_.id).applied(id).firstOption

      def findScheduleById(id: Int)(implicit s: Session) =
        schedules.findBy(_.id).applied(id).firstOption
    }
  }

  object ShipmentsModule extends AbstractModule {
    import OrdersModule._, ShipmentStatuses._

    object ShipmentStatuses {

      sealed abstract class ShipmentStatus private[ShipmentStatuses](val value: Int)
      case object Pending extends ShipmentStatus(1)
      case object Shipped extends ShipmentStatus(2)

      def fromInt(value: Int) = value match {
        case 1 ⇒ Pending
        case 2 ⇒ Shipped
      }
    }

    case class Shipment(id: Option[Int],
                        orderId: Int,
                        created: DateTime,
                        status: ShipmentStatus)

    trait ShipmentsAspect extends Structure { self: PersistentUniverse with OrdersAspect ⇒
      import profile.simple._

      class Shipments(tag: Tag) extends Table[Shipment](tag, "Shipment") {
        def id      = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def orderId = column[Int]("orderId", O.NotNull)
        def created = column[DateTime]("created", O.NotNull)
        def status  = column[ShipmentStatus]("status", O.NotNull)

        def order   = foreignKey("orderFk", orderId, orders)(_.id)

        def * = (id ?, orderId, created, status) <> (Shipment.tupled, Shipment.unapply)

        implicit val shipmentStatus =
          MappedColumnType.base[ShipmentStatus, Int](_.value, ShipmentStatuses.fromInt)
      }

      val shipments       = TableQuery[Shipments]
      val shipmentInserts = shipments returning shipments.map(_.id)

      // Can there ever be more than one shipment per any given order?
      def insertShipment(order: Order)
                        (implicit s: Session) =
        shipmentInserts += Shipment(None, order.id.get, new DateTime, Pending)

      def findShipmentById(id: Int)(implicit s: Session) =
        shipments.findBy(_.id).applied(id).firstOption
    }
  }

  object PersistenceModule {
    import CustomersModule._, TransactionsModule._, DepositsModule._,
           ProductsModule._, AccountsModule._, SubscriptionsModule._,
           OrdersModule._, TasksModule._, ShipmentsModule._

    class UnifiedPersistence(val profile: JdbcProfile) extends Foundation
      with CustomersAspect
      with TransactionsAspect
      with DepositsAspect
      with ProductsAspect
      with AccountsAspect
      with SubscriptionsAspect
      with OrdersAspect
      with TasksAspect
      with ShipmentsAspect
  }
}

object Program extends App {
  import slick.jdbc.JdbcBackend.Database
  import slick.driver.MySQLDriver.simple._
  import Domain.PersistenceModule._

  val database = Database forURL (
           url = "jdbc:mysql://localhost:3306/subscriptions",
        driver = "com.mysql.jdbc.Driver",
          user = "root",
      password = "")
  val universe = new UnifiedPersistence(slick.driver.MySQLDriver)

  database withSession { implicit session ⇒
    val x = universe.findAuthenticatedAccount("pa", "spiselkrok1")
    x.firstOption foreach println
  }
}