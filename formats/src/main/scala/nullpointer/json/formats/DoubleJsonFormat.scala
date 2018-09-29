package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonNumber, JsonValue}
import nullpointer.json.formats.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}

import scala.util.{Failure, Success, Try}

object DoubleJsonFormat extends JsonFormat[Double] {
  override def serialize(obj: Double): Try[JsonValue] =
    if (isDefined(obj))
      Success(serializeIfDefined(obj))
    else
      Failure(JsonSerializationException(s"Cannot serialize $obj"))

  private def isDefined(double: Double): Boolean =
    !double.isNaN && !double.isInfinite

  private def serializeIfDefined(double: Double): JsonValue = JsonNumber(double)

  override def deserialize(json: JsonValue): Try[Double] = json match {
    case JsonNumber(value) => Success(value)
    case _ => Failure(JsonDeserializationException(s"$json cannot be deserialized as Double"))
  }
}
