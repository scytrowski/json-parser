package nullpointer.json.parser

import nullpointer.json.JsonTokens.JsonToken
import nullpointer.json.JsonValues.JsonValue

import scala.util.Try

object JsonParser {
  def parse(tokens: Stream[JsonToken]): Try[JsonValue] =
    eatValue(tokens).map(_.value)

  private def eatValue(tokens: Stream[JsonToken]): Try[FoundValue] =
    GeneralTokenEater.eat(tokens)

  case class JsonParsingException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
}
