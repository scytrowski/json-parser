package nullpointer.json

import nullpointer.json.JsonValues.JsonValue

import scala.util.Try

trait JsonFormat[T] {
  def serialize(obj: T): Try[JsonValue]

  def deserialize(json: JsonValue): Try[T]
}
