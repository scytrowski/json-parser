package nullpointer.json.parser
import nullpointer.json.JsonValues.{JsonObject, JsonValue}
import nullpointer.json.parser.JsonParser.JsonParsingException
import nullpointer.json.tokenizer.JsonTokens._

import scala.util.{Failure, Success, Try}

object ObjectTokenEater extends TokenEater {
  override def eat(tokens: Stream[JsonToken]): Try[FoundValue] =
    if (tokens.nonEmpty)
      eatObject(tokens)
    else
      Failure(JsonParsingException("Tokens input cannot be empty"))

  private def eatObject(tokens: Stream[JsonToken]): Try[FoundValue] =
    tokens match {
      case CurlyBracketOpenToken #:: tokensTail =>
        eatObjectElements(tokensTail)
          .map { foundElements =>
            val jsonObject = JsonObject(foundElements.elements)
            FoundValue(foundElements.tokensLeft, jsonObject)
          }
      case _ => Failure(JsonParsingException("JSON object must start with {"))
    }

  private def eatObjectElements(tokens: Stream[JsonToken], elements: Map[String, JsonValue] = Map.empty): Try[FoundObjectElements] =
    tokens match {
      case StringToken(elementKey) #:: ColonToken #:: restTokens =>
        GeneralTokenEater.eat(restTokens)
          .flatMap(foundElement => eatObjectElementsSeparator(foundElement.tokensLeft, elements + (elementKey -> foundElement.value)))
      case CurlyBracketCloseToken #:: tokensTail => Success(FoundObjectElements(tokensTail, elements))
      case _ => Failure(JsonParsingException("Illegal object key-value tokens sequence"))
    }

  private def eatObjectElementsSeparator(tokens: Stream[JsonToken], elements: Map[String, JsonValue]): Try[FoundObjectElements] =
    tokens match {
      case headToken #:: tokensTail =>
        headToken match {
          case ComaToken => eatObjectElements(tokensTail, elements)
          case CurlyBracketCloseToken => Success(FoundObjectElements(tokensTail, elements))
          case otherToken => Failure(JsonParsingException(s"Unexpected token $otherToken"))
        }
      case Stream.Empty => Failure(JsonParsingException("JSON object must end with }"))
    }

  private case class FoundObjectElements(tokensLeft: Stream[JsonToken], elements: Map[String, JsonValue])
}
