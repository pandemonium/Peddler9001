package paermar
package service

// Ofrenda2014


/*
Somehow transform a plain Json-list into one that is compatible
with a WATable Data Structure (it has cols and rows according to
a specifed format). Ought to be possible to transform a
RootJsonFormat to a WATableDataStructure?
*/


import akka.actor._
import akka.io.IO

import scala.concurrent.{Promise, ExecutionContext, Future}
import scala.util.control.NonFatal

import scala.slick.jdbc.JdbcBackend._
import scala.slick.driver.MySQLDriver

import org.joda.time.format.ISODateTimeFormat
import org.joda.time.DateTime

import spray.can.Http
import spray.json._
import spray.httpx.SprayJsonSupport._
import spray.httpx.marshalling.Marshaller
import spray.httpx.unmarshalling.{MalformedContent, FromStringDeserializer}
import spray.routing._, Directives._
import spray.http.StatusCodes._
import spray.httpx.TwirlSupport
import spray.http.MediaType
import spray.http.HttpHeaders.Accept
import paermar.utility.UnitOfWork.UnitOfWork
import paermar.ui.html

object Service {
  import paermar.watable.WATable.TransportFormatProtocol.Transfer
  import paermar.application.ApplicationFeatures
  import paermar.model.Domain._
  import paermar.utility._, Parcel._
  import PersistenceModule._, TransactionsModule._, DepositsModule._,
         CustomersModule._, ProductsModule._, SubscriptionsModule._,
         OrdersModule._, TasksModule._

  trait ServiceUniverse extends ResponseSupport with TwirlSupport {
    val protocol: Protocol
    val application: ApplicationFeatures
    implicit def executionContext: ExecutionContext
    implicit def actorRefFactory: ActorContext

    implicit def runUnitOfWork[R](work: UnitOfWork[R]): String ⊕ R = application run work
  }

  trait ResponseSupport {
    def internalServerError(ctx: RequestContext)(description: String) =
      ctx.complete(InternalServerError, description)

    def accept(mediaType: MediaType) = headerValuePF({
      case Accept(range) ⇒ range
    }) require (_.head == mediaType)
  }

  trait RouteSource {
    def route: Route
  }

  class Protocol(val features: ApplicationFeatures) extends DefaultJsonProtocol {
    import TransactionType._, OrderStatus._, TaskStatuses._, UnitTypes._

    implicit object _StringToDateTime extends FromStringDeserializer[DateTime] {
      def apply(source: String) =
        try Right(_DateTimeFormat.Format parseDateTime source)
        catch {
          case NonFatal(x) ⇒
            Left(MalformedContent(s"Un-parsable date `$source`", x))
        }
    }

    implicit object _DateTimeFormat extends JsonFormat[DateTime] {
      final val Format = ISODateTimeFormat.dateTimeNoMillis.withZoneUTC

      def read(json: JsValue): DateTime = json match {
        // todo: handle errors
        case JsString(source) ⇒ Format parseDateTime source
      }

      def write(value: DateTime): JsValue = JsString(Format print value)
    }

    case class SingletonMapExtractor[K, V](key: K) {
      def unapply(m: Map[K, V]) = m get key
    }

    implicit object _TransactionTypeFormat extends JsonFormat[TransactionType] {
      def read(source: JsValue): TransactionType = source match {
        case JsString("Debit")  ⇒ Debit
        case JsString("Credit") ⇒ Credit
      }

      def write(`type`: TransactionType) = `type` match {
        case Debit  ⇒ JsString("Debit")
        case Credit ⇒ JsString("Credit")
      }
    }

    implicit object _OrderStatusFormat extends JsonFormat[OrderStatus] {
      def read(source: JsValue): OrderStatus = source match {
        case JsString("New") ⇒ New
      }

      def write(status: OrderStatus): JsValue = status match {
        case New ⇒ JsString("New")
      }
    }

    implicit object _TaskStatusFormat extends JsonFormat[TaskStatus] {
      def read(source: JsValue): TaskStatus = source match {
        case JsString("Open")   ⇒ Open
        case JsString("Closed") ⇒ Closed
      }

      def write(status: TaskStatus): JsValue = status match {
        case Open ⇒ JsString("Open")
        case Closed ⇒ JsString("Closed")
      }
    }

    implicit object _UnitTypeFormat extends JsonFormat[UnitType] {
      def read(source: JsValue): UnitType = source match {
        case JsString("Day")   ⇒ Day
        case JsString("Week")  ⇒ Week
        case JsString("Month") ⇒ Month
        case JsString("Year")  ⇒ Year
      }

      def write(status: UnitType): JsValue = status match {
        case Day   ⇒ JsString("Day")
        case Week  ⇒ JsString("Week")
        case Month ⇒ JsString("Month")
        case Year  ⇒ JsString("Year")
      }
    }

    implicit val _plainTaskFormat     = jsonFormat3(PlainTask)
    implicit val _scheduledTaskFormat = jsonFormat4(ScheduledTask)
    implicit val _taskFormat          = jsonFormat9(Task)
    implicit val _cashPaymentFormat   = jsonFormat3(CashVerification)
    implicit val _bankGiroFormat      = jsonFormat5(BankGiroVerification)
    implicit val _depositFormat       = jsonFormat9(Deposit)
    implicit val _customerFormat      = jsonFormat3(Customer)
    implicit val _transactionFormat   = jsonFormat6(Transaction)
    implicit val _productFormat       = jsonFormat5(Product)
    implicit val _subscriptionFormat  = jsonFormat5(Subscription)
    implicit val _orderFormat         = jsonFormat5(Order)
    implicit val _orderInsertFormat   = jsonFormat2(NewOrder)
    implicit val _claimFormat         = jsonFormat3(CreditCustomerDeposit)

    implicit def _ParcelFormat[X: JsonFormat, A: JsonFormat] = new RootJsonFormat[X ⊕ A] {
      val Success = SingletonMapExtractor[String, JsValue]("success")
      val Failure = SingletonMapExtractor[String, JsValue]("failure")
      val Empty   = SingletonMapExtractor[String, JsValue]("failure")

      def read(json: JsValue): X ⊕ A = json match {
        case Success(v) ⇒ successful[X, A](v.convertTo[A])
        case Failure(v) ⇒ failed[X, A]    (v.convertTo[X])
        case Empty(_)   ⇒ empty[X, A]
      }

      def write(parcel: X ⊕ A): JsValue =
        parcel.fold(x ⇒ JsObject("failure" → x.toJson),
                    s ⇒ JsObject("success" → s.toJson),
                         JsObject("empty"   → Seq.empty[String].toJson))
    }
  }

  trait ProtectedRoute extends RouteSource { self: ServiceUniverse ⇒
    import authentication._
    // Does this need a sealRoute added?

    val authenticator: UserPassAuthenticator[application.AuthenticationContext] = {
      case Some(UserPass(user, pass)) ⇒
        Future(application.authenticate(user, pass).mapOrElse(Option.apply)(None))
      case _ ⇒ Promise successful None future
    }

    override abstract def route = authenticate(BasicAuth(authenticator, realm = "Inside")) { authenticationContext ⇒
      // how do I pass on `authenticationContext` ?
      // I could probably just pass it on to route
      super.route
    }
  }

  trait ServerInfoRoute extends RouteSource { self: ServiceUniverse ⇒
    override abstract def route = thisRoute ~ super.route

    private def thisRoute = path("server-info") {
      get {
        complete {
          "Pärmar/0.1"
        }
      }
    }
  }

  trait CustomerRoute extends RouteSource { self: ServiceUniverse ⇒
    import protocol._, paermar.ui._

    override abstract def route = thisRoute ~ super.route

    private def thisRoute = pathPrefix("customers") {
      uiRoute ~ uniqueRoute ~ collectionRoute
    }

    private def collectionRoute = pathEnd {
      get {
        parameter('format ?) { format ⇒ ctx ⇒
          format match {
            case Some("watable") ⇒

              // Todo: invent way to turn the failure side of a parcel into
              // a useful DataStructure (or something) so that the client side
              // can present some kind of error box.
              application.customers fold(_    ⇒ Transfer(Map.empty, Seq.empty[String]),
                                         data ⇒ ctx.complete[Transfer[Customer]](data),
                                         Transfer(Map.empty, Seq.empty[String]))


            case _               ⇒
              ctx complete application.run(application.customers)
          }
        }
      } ~ post {
        entity(as[String]) { name ⇒
          complete(application.run(application addCustomer name))
        }
      }
    }

    private def uniqueRoute = pathPrefix(IntNumber) { customerId ⇒
      pathEnd {
        get { ctx ⇒
          (application customer customerId).fold(internalServerError(ctx),
                                                 ctx.complete[Customer],
                                                 ctx complete NotFound)
        }
      } ~ pathSuffix("orders") {
        get {
          complete(s"order: $customerId / lines")
        }
      }
    }

    private def uiRoute = get { ctx ⇒
      application.customers fold(error     ⇒ ctx complete error /*html.errorPage(error)*/,
                                 customers ⇒ ctx complete html.customers(customers),
                                 ctx complete NotFound)
    }
  }

  trait TransactionRoute extends RouteSource { self: ServiceUniverse ⇒
    import protocol._

    override abstract def route = thisRoute ~ super.route

    private def thisRoute = pathPrefix("transactions") {
      uniqueRoute ~ collectionRoute
    }

    private def collectionRoute = pathEnd {
      get {
        parameters('from.as[DateTime], 'through.as[DateTime]) { (from, through) ⇒
          complete(application run application.transactionsSpanning(from, through))
        }
      } ~ post {
        entity(as[Transaction]) { transaction ⇒
          complete(application.run(application addTransaction transaction))
        }
      }
    }

    private def uniqueRoute = pathPrefix(IntNumber) { transactionId ⇒
      get {
        complete(application.run(application transaction transactionId))
      }
    }
  }

  trait DepositRoute extends RouteSource { self: ServiceUniverse ⇒
    import protocol._

    override abstract def route = thisRoute ~ super.route

    private def thisRoute = pathPrefix("deposits") {
      uniqueRoute ~ collectionRoute
    }

    private def collectionRoute = pathEnd {
      get {
        parameters('from.as[DateTime], 'through.as[DateTime]) { (from, through) ⇒
          complete(application run application.depositsSpanning(from, through))
        }
      } ~ post {
        entity(as[CashVerification]) { verification ⇒
          complete(application.run(application addDeposit verification))
        } ~ entity(as[BankGiroVerification]) { verification ⇒
          complete(application.run(application addDeposit verification))
        }
      }
    }

    private def uniqueRoute = pathPrefix(IntNumber) { depositId ⇒
      pathEnd {
        get {
          complete(application.run(application deposit depositId))
        }
      } ~ path("customer" / IntNumber) { customerId ⇒
        put {
          entity(as[Option[String]]) { comment ⇒
            val credit = CreditCustomerDeposit(customerId, depositId, comment)

            complete(application.run(application creditDepositToCustomer credit))
          }
        }
      }
    }
  }

  trait ProductRoute extends RouteSource { self: ServiceUniverse ⇒
    import protocol._

    override abstract def route = thisRoute ~ super.route

    private def thisRoute = path("products") {
      get {
        complete(application run application.products)
      } ~ post {
        entity(as[Product]) { product ⇒
          complete(application.run(application addProduct product))
        }
      }
    }
  }

  trait SubscriptionsRoute extends RouteSource { self: ServiceUniverse ⇒
    import protocol._

    override abstract def route = thisRoute ~ super.route

    private def thisRoute = path("subscriptions") {
      get {
        complete(application run application.subscriptions)
      } ~ post {
        entity(as[Subscription]) { subscription ⇒
          complete(application.run(application addSubscription subscription))
        }
      }
    }
  }

  trait OrdersRoute extends RouteSource { self: ServiceUniverse ⇒
    import protocol._, paermar.ui._

    override abstract def route = thisRoute ~ super.route

    private def thisRoute = pathPrefix("orders") {
      uiRoute ~ uniqueRoute ~ collectionRoute
    }

    private def uiRoute = get { ctx ⇒
      application.ordersWithCustomers fold(error ⇒ ctx complete html.errorPage(error),
                                           model ⇒ ctx complete html.orders(model),
                                           ctx complete NotFound)
    }

    private def collectionRoute = pathEnd {
      get {
        // This has turned into quite the kludge
        parameter('format ?) { format ⇒ ctx ⇒
          val orders = application run application.orders
          format.fold(ctx complete orders) { fmt ⇒
            orders.fold(_    ⇒ Transfer(Map.empty, Seq.empty[String]),
                        data ⇒ ctx.complete[Transfer[Order]](data),
                        Transfer(Map.empty, Seq.empty[String]))
          }
        }
      } ~ post {
        entity(as[NewOrder]) { order ⇒
          complete(application.run(application addOrder order))
        }
      }
    }

    private def uniqueRoute = pathPrefix(IntNumber) { orderId ⇒
      pathEnd {
        get { ctx ⇒
          (application order orderId).fold(internalServerError(ctx),
                                           ctx.complete[Order],
                                           ctx complete NotFound)
        }
      } ~ pathSuffix("lines") {
        get {
          complete(s"order: $orderId / lines")
        }
      }
    }
  }

  trait TasksRoute extends RouteSource { self: ServiceUniverse ⇒
    import protocol._

    override abstract def route = thisRoute ~ super.route

    private def thisRoute = pathPrefix("tasks") {
      uiRoute ~ uniqueRoute ~ collectionRoute
    }

    private def uiRoute = get { ctx ⇒
      application.tasks fold(error ⇒ ctx complete html.errorPage(error),
                             model ⇒ ctx complete html.tasks(model),
                             ctx complete NotFound)
    }

    private def collectionRoute = pathEnd {
      get {
        // This has turned into quite the kludge
        parameter('format ?) { format ⇒ ctx ⇒
          val tasks = application run application.tasks
          format.fold(ctx complete tasks) { fmt ⇒
            tasks.fold(_    ⇒ ctx.complete(Seq.empty[String]: Transfer[String]),
                       data ⇒ ctx.complete(data: Transfer[Task]),
                       ctx.complete(Seq.empty[String]: Transfer[String]))
          }
        }
      } ~ post {
        entity(as[PlainTask]) { task ⇒
          complete(application.run(application addTask task))
        } ~  entity(as[ScheduledTask]) { task ⇒
          complete(application.run(application addTask task))
        }
      }
    }

    private def uniqueRoute = pathPrefix(IntNumber) { taskId ⇒
      pathEnd {
        get { ctx ⇒
          (application task taskId).fold(ctx.complete(InternalServerError, _),
                                         ctx.complete[Task],
                                         ctx complete NotFound)
        }
      } ~ pathSuffix("activities") {
        get {
          complete(s"task: $taskId / activities")
        }
      } ~ fieldRoutes
    }

    private def fieldRoutes = put {
      pathPrefix(Segment) {
        // How does this happen with a minimum of boiler plate?
        case "bar" ⇒ complete("Hello")
      }
    }
  }

  trait WebResourceRoute extends RouteSource { self: ServiceUniverse ⇒
    override abstract def route = thisRoute ~ super.route

    private def thisRoute = pathPrefix("") {
      getFromResourceDirectory("web-root")
    }
  }

  trait ServicePlatform extends ServiceUniverse with RouteSource {
    def route: Route = complete(NotFound)
  }

  trait ServiceRouteConcatenation extends ServicePlatform
    with ServerInfoRoute
    with ProtectedRoute
    with CustomerRoute
    with TransactionRoute
    with DepositRoute
    with ProductRoute
    with SubscriptionsRoute
    with OrdersRoute
    with TasksRoute
    with WebResourceRoute

  case class Endpoint(host: String, port: Int)

  class Router() extends HttpServiceActor with ServiceRouteConcatenation {
    val database = Database forURL (     url = "jdbc:mysql://localhost:3306/subscriptions",
                                      driver = "com.mysql.jdbc.Driver",
                                        user = "root",
                                    password = "")

    implicit def executionContext: ExecutionContext = context.dispatcher

    val application = new ApplicationFeatures(new UnifiedPersistence(MySQLDriver), database)
    val protocol    = new Protocol(application)

    def receive     = runRoute(route)
  }

  def bind(e: Endpoint)(implicit system: ActorSystem) = IO(Http) ! Http.Bind(
    listener  = system actorOf Props[Router],
    interface = e.host,
    port      = e.port)
}

object ServiceRunner extends App {
  implicit val actorSystem = ActorSystem("services")

  Service bind Service.Endpoint("localhost", 8080)
}