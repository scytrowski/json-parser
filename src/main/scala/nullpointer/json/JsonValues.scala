package nullpointer.json

object JsonValues {
  sealed trait JsonValue

  case object JsonNull extends JsonValue
  case class JsonBoolean(value: Boolean) extends JsonValue
  case class JsonNumber(value: Double) extends JsonValue
  case class JsonString(value: String) extends JsonValue

  case class JsonArray(elements: List[JsonValue]) extends JsonValue

  object JsonArray {
    def apply(): JsonArray = JsonArray(List.empty)

    def apply(firstElement: JsonValue, rest: JsonValue*): JsonArray = JsonArray(firstElement +: rest.toList)
  }

  case class JsonObject(elements: Map[String, JsonValue]) extends JsonValue

  object JsonObject {
    def apply(): JsonObject = JsonObject(Map.empty[String, JsonValue])

    def apply(firstElement: (String, JsonValue), rest: (String, JsonValue)*): JsonObject = JsonObject(Map[String, JsonValue](firstElement +: rest:_*))
  }
}
