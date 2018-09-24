package nullpointer.json.parser

import nullpointer.json.JsonTokens._
import nullpointer.json.JsonValues.{JsonArray, JsonValue}
import nullpointer.json.parser.JsonParser.JsonParsingException

import scala.util.{Failure, Success, Try}

object ArrayTokenEater extends TokenEater {
  override def eat(tokens: Stream[JsonToken]): Try[FoundValue] =
    if (tokens.nonEmpty)
      eatArray(tokens)
    else
      Failure(JsonParsingException("Tokens input cannot be empty"))

  private def eatArray(tokens: Stream[JsonToken]): Try[FoundValue] =
    tokens match {
      case SquareBracketOpenToken #:: tokensTail =>
        eatArrayElements(tokensTail)
          .map { foundElements =>
            val array = JsonArray(foundElements.elements.toList)
            FoundValue(foundElements.tokensLeft, array)
          }
      case _ => Failure(JsonParsingException("JSON array must start with ["))
    }

  private def eatArrayElements(tokens: Stream[JsonToken], elements: Seq[JsonValue] = Seq.empty): Try[FoundArrayElements] =
    tokens match {
      case SquareBracketCloseToken #:: tokensTail => Success(FoundArrayElements(tokensTail, elements))
      case _ =>
        GeneralTokenEater.eat(tokens)
          .flatMap(foundElement => eatArrayElementsSeparator(foundElement.tokensLeft, elements :+ foundElement.value))
    }

  private def eatArrayElementsSeparator(tokens: Stream[JsonToken], elements: Seq[JsonValue]): Try[FoundArrayElements] =
    tokens match {
      case headToken #:: tokensTail =>
        headToken match {
          case ComaToken => eatArrayElements(tokensTail, elements)
          case SquareBracketCloseToken => Success(FoundArrayElements(tokensTail, elements))
          case otherToken => Failure(JsonParsingException(s"Unexpected token $otherToken"))
        }
      case Stream.Empty => Failure(JsonParsingException("JSON array must end with ]"))
    }

  private case class FoundArrayElements(tokensLeft: Stream[JsonToken], elements: Seq[JsonValue])
}
