package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonFormatExceptions.JsonDeserializationException
import nullpointer.json.JsonValues.{JsonBoolean, JsonValue}

import scala.util.{Failure, Success, Try}

object BooleanJsonFormat extends JsonFormat[Boolean] {
  override def serialize(obj: Boolean): Try[JsonValue] = Success(JsonBoolean(obj))

  override def deserialize(json: JsonValue): Try[Boolean] = json match {
    case JsonBoolean(value) => Success(value)
    case _ =>
      Failure(JsonDeserializationException(s"Cannot deserialize boolean from $json"))
  }
}
