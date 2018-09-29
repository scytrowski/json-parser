package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.JsonValue
import nullpointer.json.formats.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}

import scala.util.{Failure, Success, Try}

class TryJsonFormat[R](implicit resultFormat: JsonFormat[R]) extends JsonFormat[Try[R]] {
  override def serialize(obj: Try[R]): Try[JsonValue] = obj match {
    case Success(value) => resultFormat.serialize(value)
    case Failure(exception) =>
      Failure(JsonSerializationException("Cannot serialize failed try", exception))
  }

  override def deserialize(json: JsonValue): Try[Try[R]] = resultFormat.deserialize(json) match {
    case success: Success[R] => Success(success)
    case Failure(exception) =>
      Failure(JsonDeserializationException("Cannot deserialize try due result deserializer exception", exception))
  }
}
