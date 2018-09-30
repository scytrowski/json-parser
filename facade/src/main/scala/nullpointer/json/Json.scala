package nullpointer.json

import nullpointer.json.JsonTokens.JsonToken
import nullpointer.json.JsonValues.JsonValue
import nullpointer.json.parser.JsonParser
import nullpointer.json.tokenizer.JsonTokenizer

import scala.util.Try

object Json {
  def parse(source: String): Try[JsonValue] = {
    val tokens = tokenize(source)
    parseTokens(tokens)
  }

  def toJson[T](obj: T)(implicit format: JsonFormat[T]): Try[JsonValue] = format.serialize(obj)

  def ofJson[T](json: JsonValue)(implicit format: JsonFormat[T]): Try[T] = format.deserialize(json)

  private def tokenize(source: String): Stream[JsonToken] = JsonTokenizer.tokenize(source)

  private def parseTokens(tokens: Stream[JsonToken]): Try[JsonValue] = JsonParser.parse(tokens)
}
