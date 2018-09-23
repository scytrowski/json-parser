package nullpointer.json.parser

import nullpointer.json.JsonValues._
import nullpointer.json.tokenizer.JsonTokens._

import scala.util.Try

object JsonParser {
  def parse(tokens: Stream[JsonToken]): Try[JsonValue] =
    eatValue(tokens).map(_.value)

  private def eatValue(tokens: Stream[JsonToken]): Try[FoundValue] =
    GeneralTokenEater.eat(tokens)

  case class JsonParsingException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
}
