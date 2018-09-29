package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonNumber, JsonValue}
import nullpointer.json.JsonFormatExceptions.JsonDeserializationException

import scala.util.{Failure, Success, Try}

object ShortJsonFormat extends JsonFormat[Short] {
  override def serialize(obj: Short): Try[JsonValue] = Success(JsonNumber(obj))

  override def deserialize(json: JsonValue): Try[Short] = json match {
    case JsonNumber(value) => Success(value.toShort)
    case _ => Failure(JsonDeserializationException(s"$json cannot be deserialized as Short"))
  }
}
