package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonNumber, JsonValue}
import nullpointer.json.formats.JsonFormatExceptions.JsonDeserializationException

import scala.util.{Failure, Success, Try}

object ByteJsonFormat extends JsonFormat[Byte] {
  override def serialize(obj: Byte): Try[JsonValue] = Success(JsonNumber(obj))

  override def deserialize(json: JsonValue): Try[Byte] = json match {
    case JsonNumber(value) => Success(value.toByte)
    case _ => Failure(JsonDeserializationException(s"$json cannot be deserialized as Byte"))
  }
}
