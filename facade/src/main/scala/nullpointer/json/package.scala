package nullpointer

import nullpointer.json.JsonValues.JsonValue

import scala.util.Try

package object json {
  implicit class StringWithParse(string: String) {
    def parse: Try[JsonValue] = Json.parse(string)
  }
}
