package nullpointer.json.parser
import nullpointer.json.JsonValues.{JsonBoolean, JsonNull, JsonNumber, JsonString}
import nullpointer.json.parser.JsonParser.JsonParsingException
import nullpointer.json.tokenizer.JsonTokens._

import scala.util.{Failure, Success, Try}

object GeneralTokenEater extends TokenEater {
  override def eat(tokens: Stream[JsonToken]): Try[FoundValue] =
    tokens match {
      case headToken #:: tokensTail =>
        headToken match {
          case NullToken => Success(FoundValue(tokensTail, JsonNull))
          case FalseToken => Success(FoundValue(tokensTail, JsonBoolean(false)))
          case TrueToken => Success(FoundValue(tokensTail, JsonBoolean(true)))
          case NumberToken(value) => Success(FoundValue(tokensTail, JsonNumber(value)))
          case StringToken(value) => Success(FoundValue(tokensTail, JsonString(value)))
          case SquareBracketOpenToken => ArrayTokenEater.eat(tokens)
          case CurlyBracketOpenToken => ObjectTokenEater.eat(tokens)
          case otherToken => Failure(JsonParsingException(s"Unexpected token $otherToken"))
        }
      case Stream.Empty => Failure(JsonParsingException("Tokens input cannot be empty"))
    }
}
