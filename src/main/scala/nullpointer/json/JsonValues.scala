package nullpointer.json

object JsonValues {
  sealed trait JsonValue

  case object JsonNull extends JsonValue
  case class JsonBoolean(value: Boolean) extends JsonValue
  case class JsonNumber(value: Double) extends JsonValue
  case class JsonString(value: String) extends JsonValue
  case class JsonArray(elements: Seq[JsonValue]) extends JsonValue
  case class JsonObject(elements: Map[String, JsonValue])
}
