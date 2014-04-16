package paermar
package model

import slick.driver.JdbcProfile
import org.joda.time.DateTime
import com.github.tototoshi.slick.MySQLJodaSupport._

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

      val findCustomerById = customers.findBy(_.id)
      def insertCustomer(name: String)(implicit s: Session): Int =
        customers returning customers.map(_.id) += Customer(None, name, currentDateTime)
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
        tx <- transactions
        if tx.created >= from && tx.created <= through
      } yield tx

      // What is the contract here? Do I just... firstOption this or leave it as is? What
      // would be the benifit to leaving it like this?
      def findTransactionById = transactions.findBy(_.id)
    }
  }

  object DepositsModule extends AbstractModule {
    import TransactionsModule._, CustomersModule._

    sealed trait Payment
    case class CashPayment(customerId: Int, amount: Int, reference: String)
      extends Payment
    case class BankGiroVerification(valueDate: DateTime,
                                    amount: Int,
                                    reference: String,
                                    payer: String,
                                    avi: String)
      extends Payment

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

        def transaction   =
          foreignKey("transactionFk", transactionId, transactions)(_.id)

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
        val projection = for (d <- deposits if d.id === deposit.id.get)
                           yield d.transactionId

        projection update transactionId
      }

      def depositsSpanning(from: DateTime, through: DateTime) = for {
        d <- deposits
        if d.created >= from && d.created <= through
      } yield d

      def findDepositById = deposits.findBy(_.id)
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

      class Products(tag: Tag) extends Table[Product](tag: Tag, "Products") {
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

      def insertProduct(`type`: Int,
                        name: String,
                        unitPrice: Int,
                        description: Option[String])
                       (implicit s: Session) =
        productInserts += Product(None, `type`, name, unitPrice, description)

      def findProductById = products.findBy(_.id)
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

      class Orders(tag: Tag) extends Table[Order](tag: Tag, "Orders") {
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

      class OrderLines(tag: Tag) extends Table[OrderLine](tag: Tag, "OrderLines") {
        def id          = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def orderId     = column[Int]("orderId", O.NotNull)
        def productId   = column[Int]("productId", O.NotNull)
        def quantity    = column[Int]("quantity", O.NotNull)
        def description = column[String]("description")

        def order       = foreignKey("orderFk", orderId, orders)(_.id)
        def product     = foreignKey("productFk", productId, products)(_.id)

        def *           = (id ?, orderId, productId, quantity, description ?) <>
          (OrderLine.tupled, OrderLine.unapply)
      }

      val orders           = TableQuery[Orders]
      val orderInserts     = orders returning orders.map(_.id)
      val orderLines       = TableQuery[OrderLines]
      val orderLineInserts = orderLines returning orderLines.map(_.id)

      def insertOrder(customer: Customer, comment: Option[String])
                     (implicit s: Session) =
        orderInserts += Order(None, customer.id.get, currentDateTime, New, comment)

      def insertOrderLine(order: Order,
                          product: Product,
                          quantity: Int,
                          description: Option[String])
                         (implicit s: Session) =
        orderLineInserts += OrderLine(None, order.id.get, product.id.get, quantity, description)

      def findOrderById     = orders.findBy(_.id)
      def findOrderLineById = orderLines.findBy(_.id)
    }
  }

  object AccountsModule extends AbstractModule {
    case class Account(id: Option[Int],
                       login: String,
                       password: String,
                       fullName: String)

    trait AccountsAspect extends Structure { self: PersistentUniverse ⇒
      import profile.simple._

      class Accounts(tag: Tag) extends Table[Account](tag: Tag, "Accounts") {
        def id       = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def login    = column[String]("login", O.NotNull)
        def password = column[String]("password", O.NotNull)
        def fullName = column[String]("fullName", O.NotNull)

        def *        = (id ?, login, password, fullName) <> (Account.tupled, Account.unapply)
      }

      val accounts = TableQuery[Accounts]

      def insertAccount(login: String, password: String, fullName: String)
                       (implicit s: Session) =
        accounts += Account(None, login, password, fullName)

      def findAuthenticatedAccount(login: String, password: String)
                                  (implicit s: Session) =
        for (acct <- accounts
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

      class Subscriptions(tag: Tag) extends Table[Subscription](tag: Tag, "Subscriptions") {
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

      def insertSubscription(customer: Customer, startDate: Option[DateTime], comment: Option[String])
                            (implicit s: Session) =
        subscriptionInsert += Subscription(None, customer.id.get, new DateTime, startDate, comment)

      def findSubscriptionById = subscriptions.findBy(_.id)
    }
  }

  object PersistenceModule {
    import CustomersModule._, TransactionsModule._, DepositsModule._,
           ProductsModule._, AccountsModule._, SubscriptionsModule._,
           OrdersModule._

    class UnifiedPersistence(val profile: JdbcProfile) extends Foundation
      with CustomersAspect
      with TransactionsAspect
      with DepositsAspect
      with ProductsAspect
      with AccountsAspect
      with SubscriptionsAspect
      with OrdersAspect
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
    val x = universe.findAuthenticatedAccount("pa", "spi1selkrok1")
    x.firstOption foreach println
  }
}
