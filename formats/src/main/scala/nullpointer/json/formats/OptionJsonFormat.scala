package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonNull, JsonValue}

import scala.util.{Success, Try}

class OptionJsonFormat[E](implicit elementFormat: JsonFormat[E]) extends JsonFormat[Option[E]] {
  override def serialize(obj: Option[E]): Try[JsonValue] = obj match {
    case Some(element) => elementFormat.serialize(element)
    case None => Success(JsonNull)
  }

  override def deserialize(json: JsonValue): Try[Option[E]] = json match {
    case JsonNull => Success(None)
    case otherJson =>
      elementFormat
        .deserialize(otherJson)
        .map(Some(_))
  }
}
