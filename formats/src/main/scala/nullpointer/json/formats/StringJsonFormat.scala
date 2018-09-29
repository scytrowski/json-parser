package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonString, JsonValue}
import nullpointer.json.JsonFormatExceptions.JsonDeserializationException

import scala.util.{Failure, Success, Try}

object StringJsonFormat extends JsonFormat[String] {
  override def serialize(obj: String): Try[JsonValue] = Success(JsonString(obj))

  override def deserialize(json: JsonValue): Try[String] = json match {
    case JsonString(value) => Success(value)
    case _ => Failure(JsonDeserializationException(s"$json cannot be deserialized as String"))
  }
}
