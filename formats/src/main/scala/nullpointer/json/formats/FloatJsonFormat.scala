package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonNumber, JsonValue}
import nullpointer.json.formats.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}

import scala.util.{Failure, Success, Try}

object FloatJsonFormat extends JsonFormat[Float] {
  override def serialize(obj: Float): Try[JsonValue] =
    if (isDefined(obj))
      Success(serializeIfDefined(obj))
    else
      Failure(JsonSerializationException(s"Cannot serialize $obj"))

  private def isDefined(float: Float): Boolean =
    !float.isNaN && !float.isInfinite

  private def serializeIfDefined(float: Float): JsonValue = JsonNumber(float)

  override def deserialize(json: JsonValue): Try[Float] = json match {
    case JsonNumber(value) => Success(value.toFloat)
    case _ => Failure(JsonDeserializationException(s"$json cannot be deserialized as Float"))
  }
}
