package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonString, JsonValue}
import nullpointer.json.formats.JsonFormatExceptions.JsonDeserializationException

import scala.util.{Failure, Success, Try}

object CharJsonFormat extends JsonFormat[Char] {
  override def serialize(obj: Char): Try[JsonValue] = Success(JsonString(obj.toString))

  override def deserialize(json: JsonValue): Try[Char] = json match {
    case JsonString(value) if value.length == 1 => Success(value.head)
    case _ => Failure(JsonDeserializationException(s"$json cannot be deserialized as Char"))
  }
}
