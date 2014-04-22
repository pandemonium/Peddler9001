package paermar
package watable

import paermar.model.Domain.OrdersModule.{OrderStatus, Order}
import java.lang.reflect.Field
import scala.reflect.ClassTag

object WATable {
  val source =
    """
      |[
      |{
      |  "id": 2,
      |  "customerId": 5,
      |  "created": "2014-04-15T10:26:49Z",
      |  "status": "New"
      |},
      |{
      |  "id": 3,
      |  "customerId": 5,
      |  "created": "2014-04-15T10:26:49Z",
      |  "status": "New"
      |}
      |]
    """.stripMargin

  val target =
    """
      |{
      |  cols: {
      |    id: {
      |      index: 1,
      |      type: "number"
      |    },
      |    customerId: {
      |      index: 2,
      |      type: "number"
      |    },
      |    created: {
      |      index: 3,
      |      type: "date"
      |    },
      |    status: {
      |      index: 4,
      |      type: "string"
      |    }
      |  },
      |  rows: [
      |    {
      |      "id": 2,
      |      "customerId": 5,
      |      "created": "2014-04-15T10:26:49Z",
      |      "status": "New"
      |    },
      |    {
      |      "id": 3,
      |      "customerId": 5,
      |      "created": "2014-04-15T10:26:49Z",
      |      "status": "New"
      |    }
      |  ]
      |}
    """.stripMargin


  import spray.json._
  object TransportFormatProtocol extends DefaultJsonProtocol {

    // See if I could use Scala picklers instead?

    /**
     * A different strategy would be to actually generate the json list
     * and extract its "star product" - looking at each field to determine
     * types based on the json productions.
     *
     * This might come with a performance penalty but could still be worth it.
     *
     * Or just "clone" sprays internal json format hierarchy.
     */

    object ColumnTypeName {
      def unapply(field: Field) = field.getType.getName match {
        case "int" | "long" | "double" | "float" ⇒ Some("number")
        case "boolean"                           ⇒ Some("bool")
        case _                                   ⇒ Some("string")
      }
    }

    case class Column(index: Int, `type`: String)
    case class Transfer[T: JsonFormat](cols: Map[String, Column], rows: Seq[T])

    implicit val _columnFormat                   = jsonFormat2(Column)
    implicit def _transportFormat[T: JsonFormat] = jsonFormat2(Transfer[T])

    def schema[T: ClassTag] = reflect.classTag[T] match {
      case tag ⇒
        (extractFieldNames(tag).zipWithIndex map { case (fieldName, index) ⇒
          fieldName -> Column(index + 1, deduceColumnType(tag.runtimeClass, fieldName))
        }).toMap
    }

    implicit def asTransfer[T: JsonFormat: ClassTag](data: Seq[T]): Transfer[T] =
      Transfer(schema[T], data)

    def deduceColumnType(runtimeClass: Class[_], field: String) = runtimeClass getDeclaredField field match {
      case ColumnTypeName(name) ⇒ name
    }
  }
}

object RunningMan extends App {
  import WATable.TransportFormatProtocol._

  object Formats extends spray.json.DefaultJsonProtocol {
    implicit val _fooFormat = jsonFormat2(Foo)
  }

  case class Foo(x: Int, y: String)

  import Formats._
  println((Seq(Foo(1, "2"), Foo(3, "4")): Transfer[Foo]).toJson)
}