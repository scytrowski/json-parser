package nullpointer.json.parser

import nullpointer.json.JsonValues.JsonValue
import nullpointer.json.tokenizer.JsonTokens.JsonToken

import scala.util.Try

object JsonParser {
  def parse(tokens: Stream[JsonToken]): Try[JsonValue] = ???
}
