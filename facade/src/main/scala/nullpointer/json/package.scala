package nullpointer

import nullpointer.json.JsonValues.JsonValue

import scala.util.Try

package object json extends BasicJsonFormats {
  implicit class StringWithParse(string: String) {
    def parse: Try[JsonValue] = Json.parse(string)
  }

  implicit class ObjectWithToJson[T: JsonFormat](obj: T) {
    def toJson: Try[JsonValue] = Json.toJson(obj)
  }

  implicit class JsonValueWithConvertTo(json: JsonValue) {
    def convertTo[T: JsonFormat]: Try[T] = Json.ofJson[T](json)
  }
}
