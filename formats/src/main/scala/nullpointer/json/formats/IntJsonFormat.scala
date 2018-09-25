package nullpointer.json.formats

import nullpointer.json.{JsonFormat, JsonValues}

import scala.util.Try

object IntJsonFormat extends JsonFormat[Int] {
  override def serialize(obj: Int): Try[JsonValues.JsonValue] = ???

  override def deserialize(json: JsonValues.JsonValue): Try[Int] = ???
}
