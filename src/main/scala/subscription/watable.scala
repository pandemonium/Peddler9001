package paermar
package watable

import paermar.model.Domain.OrdersModule.{OrderStatus, Order}
import java.lang.reflect.Field

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
  object DataStructureProtocol extends DefaultJsonProtocol {

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

    case class Column(index: Int, name: String, `type`: String)
    case class DataStructure[T: JsonFormat](cols: Seq[Column], rows: Seq[T])

    implicit val _columnFormat                       = jsonFormat3(Column)
    implicit def _dataStructureFormat[T: JsonFormat] = jsonFormat2(DataStructure[T])

    def schema[T: ClassManifest] = classManifest[T] match {
      case manifest ⇒
        extractFieldNames(manifest).zipWithIndex map { case (fieldName, index) ⇒
          Column(index + 1, fieldName, deduceType(manifest.erasure, fieldName))
        }
    }

    implicit def asDataStructure[T: JsonFormat: ClassManifest](data: Seq[T]): DataStructure[T] =
      DataStructure(schema[T], data)

    def deduceType(erasure: Class[_], fieldName: String) = erasure getDeclaredField fieldName match {
      case ColumnTypeName(name) ⇒ name
    }
  }
}

object RunningMan extends App {
  case class Foo(x: Int, y: String)
  val x = WATable.DataStructureProtocol.schema[Order]

  x foreach println
}