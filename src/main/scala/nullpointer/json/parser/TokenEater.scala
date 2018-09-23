package nullpointer.json.parser

import nullpointer.json.tokenizer.JsonTokens.JsonToken

import scala.util.Try

trait TokenEater {
  def eat(tokens: Stream[JsonToken]): Try[FoundValue]
}
