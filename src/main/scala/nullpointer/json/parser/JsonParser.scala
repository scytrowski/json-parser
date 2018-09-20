package nullpointer.json.parser

import nullpointer.json.JsonValues._
import nullpointer.json.tokenizer.JsonTokens._

import scala.util.{Failure, Success, Try}

object JsonParser {
  def parse(tokens: Stream[JsonToken]): Try[JsonValue] =
    eatValue(tokens).map(_.value)

  private def eatValue(tokens: Stream[JsonToken]): Try[FoundValue] =
    tokens match {
      case headToken #:: tokensTail =>
        headToken match {
          case NullToken => Success(FoundValue(tokensTail, JsonNull))
          case FalseToken => Success(FoundValue(tokensTail, JsonBoolean(false)))
          case TrueToken => Success(FoundValue(tokensTail, JsonBoolean(true)))
          case NumberToken(value) => Success(FoundValue(tokensTail, JsonNumber(value)))
          case StringToken(value) => Success(FoundValue(tokensTail, JsonString(value)))
          case otherToken => Failure(JsonParsingException(s"Unknown token $otherToken"))
        }
      case Stream.Empty => Failure(JsonParsingException("Tokens input is empty"))
    }

  case class FoundValue(tokensLeft: Seq[JsonToken], value: JsonValue)

  case class JsonParsingException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
}
