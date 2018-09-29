package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonNull, JsonValue}
import nullpointer.json.formats.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}

import scala.util.{Failure, Success, Try}

class OptionJsonFormat[E](implicit elementFormat: JsonFormat[E]) extends JsonFormat[Option[E]] {
  override def serialize(obj: Option[E]): Try[JsonValue] = obj match {
    case Some(element) => serializeIfDefined(element)
    case None => Success(JsonNull)
  }

  private def serializeIfDefined(obj: E): Try[JsonValue] = elementFormat.serialize(obj) match {
    case success: Success[JsonValue] => success
    case Failure(exception) =>
      Failure(JsonSerializationException("Cannot serialize option due to element serialization exception", exception))
  }

  override def deserialize(json: JsonValue): Try[Option[E]] = json match {
    case JsonNull => Success(None)
    case _ => deserializeIfDefined(json)
  }

  private def deserializeIfDefined(json: JsonValue): Try[Option[E]] = elementFormat.deserialize(json) match {
    case success: Success[E] => success.map(Some(_))
    case Failure(exception) =>
      Failure(JsonDeserializationException("Cannot deserialize option due to element deserialization exception", exception))
  }
}
