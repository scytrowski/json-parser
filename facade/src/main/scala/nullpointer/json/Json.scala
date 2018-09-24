package nullpointer.json

import nullpointer.json.JsonTokens.JsonToken
import nullpointer.json.JsonValues.JsonValue
import nullpointer.json.parser.JsonParser
import nullpointer.json.tokenizer.JsonTokenizer

import scala.util.Try

object Json {
  def parse(source: String): Try[JsonValue] = {
    val tokens = tokenize(source)
    parse(tokens)
  }

  private def tokenize(source: String): Stream[JsonToken] = JsonTokenizer.tokenize(source)

  private def parse(tokens: Stream[JsonToken]): Try[JsonValue] = JsonParser.parse(tokens)
}
