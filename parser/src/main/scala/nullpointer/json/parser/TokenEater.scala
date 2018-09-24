package nullpointer.json.parser

import nullpointer.json.JsonTokens.JsonToken

import scala.util.Try

trait TokenEater {
  def eat(tokens: Stream[JsonToken]): Try[FoundValue]
}
