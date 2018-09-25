package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonNumber, JsonValue}
import nullpointer.json.formats.JsonFormatExceptions.JsonDeserializationException

import scala.util.{Failure, Success, Try}

object DoubleJsonFormat extends JsonFormat[Double] {
  override def serialize(obj: Double): Try[JsonValue] = Success(JsonNumber(obj))

  override def deserialize(json: JsonValue): Try[Double] = json match {
    case JsonNumber(value) => Success(value)
    case _ => Failure(JsonDeserializationException(s"$json cannot be deserialized as Double"))
  }
}
