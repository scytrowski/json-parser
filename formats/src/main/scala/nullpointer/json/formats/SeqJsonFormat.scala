package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonArray, JsonValue}
import nullpointer.json.formats.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}

import scala.util.{Failure, Success, Try}

class SeqJsonFormat[E](implicit elementFormat: JsonFormat[E]) extends JsonFormat[Seq[E]] {
  override def serialize(obj: Seq[E]): Try[JsonValue] = {
    val serializedElementTries = obj.map(elementFormat.serialize)
    if (serializedElementTries.forall(_.isSuccess))
      Success(JsonArray(serializedElementTries.map(_.get).toList))
    else
      Failure(JsonSerializationException("Cannot serialize sequence due to one of elements serialization exception", serializedElementTries.find(_.isFailure).get.asInstanceOf[Failure[JsonValue]].exception))
  }

  override def deserialize(json: JsonValue): Try[Seq[E]] = json match {
    case jsonArray: JsonArray => deserializeJsonArray(jsonArray)
    case _ => Failure(JsonDeserializationException(s"Cannot deserialize sequence of $json"))
  }

  private def deserializeJsonArray(jsonArray: JsonArray): Try[Seq[E]] = {
    val deserializedElementTries = jsonArray.elements.map(elementFormat.deserialize)
    if (deserializedElementTries.forall(_.isSuccess))
      Success(deserializedElementTries.map(_.get))
    else
      Failure(JsonDeserializationException("Cannot deserialize sequence due to one of elements deserialization exception", deserializedElementTries.find(_.isFailure).get.asInstanceOf[Failure[E]].exception))
  }
}
