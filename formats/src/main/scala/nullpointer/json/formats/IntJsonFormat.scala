package nullpointer.json.formats

import nullpointer.json.JsonValues.{JsonNumber, JsonValue}
import nullpointer.json.formats.JsonFormatExceptions.JsonDeserializationException
import nullpointer.json.{JsonFormat, JsonValues}

import scala.util.{Failure, Success, Try}

object IntJsonFormat extends JsonFormat[Int] {
  override def serialize(obj: Int): Try[JsonValues.JsonValue] = Success(JsonNumber(obj))

  override def deserialize(json: JsonValue): Try[Int] = json match {
    case JsonNumber(value) => Success(value.toInt)
    case _ => Failure(JsonDeserializationException(s"$json cannot be deserialized as Int"))
  }
}
