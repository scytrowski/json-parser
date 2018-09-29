package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonArray, JsonValue}
import nullpointer.json.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}

import scala.util.{Failure, Success, Try}

class SeqJsonFormat[E](implicit elementFormat: JsonFormat[E]) extends JsonFormat[Seq[E]] {
  import SeqOfTriesExtensions.SeqOfTriesWithAllSucceeded

  override def serialize(obj: Seq[E]): Try[JsonValue] =
    obj.map(elementFormat.serialize).allSucceeded match {
      case Success(serializedElements) => Success(JsonArray(serializedElements.toList))
      case Failure(exception) =>
        Failure(JsonSerializationException("Cannot serialize sequence due to one of elements serialization exception", exception))
    }

  override def deserialize(json: JsonValue): Try[Seq[E]] = json match {
    case jsonArray: JsonArray => deserializeJsonArray(jsonArray)
    case _ => Failure(JsonDeserializationException(s"Cannot deserialize sequence from $json"))
  }

  private def deserializeJsonArray(jsonArray: JsonArray): Try[Seq[E]] =
    jsonArray.elements.map(elementFormat.deserialize).allSucceeded match {
      case success: Success[Seq[E]] => success
      case Failure(exception) =>
        Failure(JsonDeserializationException("Cannot deserialize sequence due to one of elements deserialization exception", exception))
    }
}
