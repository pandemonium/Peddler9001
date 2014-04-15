package paermar
package watable

object Watable {
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
      |},
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
      |    },
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
      |    },
      |  ]
      |}
    """.stripMargin
}