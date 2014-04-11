package paermar
package service

// Ofrenda2014

import akka.actor._
import akka.io.IO
import paermar.application.ApplicationFeatures
import paermar.model.Domain
import paermar.model.Domain.UnifiedPersistence
import spray.can.Http
import scala.concurrent.{Promise, ExecutionContext, Future}
import scala.slick.jdbc.JdbcBackend._
import scala.slick.driver.MySQLDriver
import spray.json._
import spray.httpx.SprayJsonSupport._
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.DateTime
import spray.httpx.unmarshalling.{MalformedContent, Deserializer, FromStringDeserializer, DeserializationError}
import scala.util.control.NonFatal

object Service {
  import spray.routing._
  import Directives._

  trait ServiceUniverse {
    val protocol: Protocol
    val application: ApplicationFeatures
    implicit def executionContext: ExecutionContext
  }

  trait RouteLike {
    def route: Route
  }

  class Protocol(val features: ApplicationFeatures) extends DefaultJsonProtocol {
    import features.persistence._, Domain._, application.Parcel._

    type UP = UnifiedPersistence

    implicit object String2DateFormat extends FromStringDeserializer[DateTime] {
      def apply(source: String) =
        try
          Right(_DateTimeFormat.Format parseDateTime source)
        catch {
          case NonFatal(x) ⇒ Left(MalformedContent(x.getMessage))
        }
    }

    implicit object _DateTimeFormat extends JsonFormat[DateTime] {
      final val Format = ISODateTimeFormat.dateTimeNoMillis.withZoneUTC

      def read(json: JsValue): DateTime = json match {
        case JsString(s) ⇒
          // todo: handle errors
          Format parseDateTime s
      }

      def write(value: DateTime): JsValue = JsString(Format.print(value))
    }

    case class SingletonMapExtractor[K, V](key: K) {
      def unapply(m: Map[K, V]) = m get key
    }

    implicit object _TransactionTypeFormat extends JsonFormat[TransactionType.TransactionType] {
      def read(source: JsValue): TransactionType.TransactionType = source match {
        case JsString("Debit") ⇒ TransactionType.Debit
        case JsString("Credit") ⇒ TransactionType.Credit
      }
      def write(`type`: TransactionType.TransactionType) = `type` match {
        case TransactionType.Debit  ⇒ JsString("Debit")
        case TransactionType.Credit ⇒ JsString("Credit")
      }
    }

    implicit val _depositFormat:  RootJsonFormat[UP#Deposit]  = jsonFormat9(Deposit)
    implicit val _customerFormat: RootJsonFormat[UP#Customer] = jsonFormat3(Customer)
    implicit val _transactionFormat: RootJsonFormat[UP#Transaction] = jsonFormat6(Transaction)

    implicit def _ParcelFormat[X: JsonFormat, A: JsonFormat] = new RootJsonFormat[X ⊕ A] {
      val Success = SingletonMapExtractor[String, JsValue]("success")
      val Failure = SingletonMapExtractor[String, JsValue]("failure")

      def read(json: JsValue): X ⊕ A = json match {
        case Success(v) ⇒ successful[X, A](v.convertTo[A])
        case Failure(v) ⇒ failed[X, A]    (v.convertTo[X])
      }

      def write(parcel: X ⊕ A): JsValue =
        parcel.fold(x ⇒ JsObject("failure" -> x.toJson),
                    s ⇒ JsObject("success" -> s.toJson))
    }
  }

  trait ProtectedRoute extends RouteLike { self: ServiceUniverse ⇒
    import authentication._
    // Does this need a sealRoute added?

    val authenticator: UserPassAuthenticator[application.AuthenticationContext] = {
      case Some(UserPass(user, pass)) ⇒
        Future(application.authenticate(user, pass).mapOrElse(Option.apply)(None))
      case _ ⇒ Promise successful None future
    }

    override abstract def route = authenticate(BasicAuth(authenticator, realm = "Inside")) { authenticationContext ⇒
      // how do I pass on `authenticationContext` ?
      super.route
    }
  }

  trait ServerInfoRoute extends RouteLike { self: ServiceUniverse ⇒
    override abstract def route = thisRoute ~ super.route

    private def thisRoute = path("server-info") {
      get {
        complete {
          "Pärmar/0.1"
        }
      }
    }
  }

  trait CustomerRoute extends RouteLike { self: ServiceUniverse ⇒
    import protocol._

    override abstract def route = thisRoute ~ super.route

    private def thisRoute = path("customers") {
      get {
        complete(application.customers)
      } ~ post {
        entity(as[String]) { name ⇒ ctx ⇒
          val customer = application addCustomer name

          println(s"name: `$name`")

          ctx complete customer
        }
      }
    }
  }

  trait TransactionRoute extends RouteLike { self: ServiceUniverse ⇒
    import protocol._

    override abstract def route = thisRoute ~ super.route

    private def thisRoute = path("transactions") {
      get {
        parameters('from.as[DateTime], 'through.as[DateTime]) { (from, through) ⇒
          complete(application.transactions(from, through))
        }
      } ~ post {
        entity(as[UP#Transaction]) { transaction ⇒
          val created = application addTransaction transaction

          complete(created)
        }
      }
    }
  }

  trait ServicePlatform extends ServiceUniverse with RouteLike {
    def route: Route = reject
  }

  trait ServiceRouteConcatenation extends ServicePlatform
    with ServerInfoRoute
    with ProtectedRoute
    with CustomerRoute
    with TransactionRoute

  case class Endpoint(host: String, port: Int)

  class Router() extends HttpServiceActor with ServiceRouteConcatenation {
    val database = Database forURL (     url = "jdbc:mysql://localhost:3306/subscriptions",
                                      driver = "com.mysql.jdbc.Driver",
                                        user = "root",
                                    password = "")

    lazy val application: ApplicationFeatures =
      new ApplicationFeatures(new UnifiedPersistence(MySQLDriver), database)

    val protocol = new Protocol(application)

    implicit def executionContext: ExecutionContext = context.dispatcher
    def receive = runRoute(route)
  }

  def bind(e: Endpoint)(implicit system: ActorSystem) = IO(Http) ! Http.Bind(
    listener  = system actorOf Props[Router],
    interface = e.host,
    port      = e.port)
}

object ServiceRunner extends App {
  implicit val actorSystem = ActorSystem("services")

  Service bind Service.Endpoint("localhost", 8080)

  Console.in.read()
  actorSystem.shutdown()
}