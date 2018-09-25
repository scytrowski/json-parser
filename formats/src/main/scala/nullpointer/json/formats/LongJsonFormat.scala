package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonNumber, JsonValue}
import nullpointer.json.formats.JsonFormatExceptions.JsonDeserializationException

import scala.util.{Failure, Success, Try}

object LongJsonFormat extends JsonFormat[Long] {
  override def serialize(obj: Long): Try[JsonValue] = Success(JsonNumber(obj))

  override def deserialize(json: JsonValue): Try[Long] = json match {
    case JsonNumber(value) => Success(value.toLong)
    case _ => Failure(JsonDeserializationException(s"$json cannot be deserialized as Long"))
  }
}
