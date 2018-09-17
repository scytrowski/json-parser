package nullpointer.json

import nullpointer.json.JsonTokens._

import scala.util.matching.Regex

class JsonTokenizer {
  import JsonTokenizer._

  def tokenize(source: String): Stream[JsonToken] =
    Stream.iterate(FoundToken.startOfSource(source)) {
      case FoundToken(sourceLeft, _) =>
        if (!sourceLeft.isEmpty)
          (sourceLeft #:: sourceLeft.tails.toStream)
            .map(findToken)
            .dropWhile(_.isEmpty)
            .map(_.get)
            .headOption
            .getOrElse(FoundToken.endOfSource)
        else
          FoundToken.endOfSource
    }.tail.map(_.token)

  private def findToken(source: String): Option[FoundToken] =
    findCharDefinedToken(source)
      .orElse(findStringToken(source))
      .orElse(findNumberToken(source))
      .orElse(findStringDefinedToken(source))

  private def findCharDefinedToken(source: String): Option[FoundToken] =
    charDefinedTokens
      .get(source.head)
      .map(t => FoundToken(source.tail, t))

  private def findStringDefinedToken(source: String): Option[FoundToken] =
    stringDefinedTokens
      .view
      .map { case (tokenString, token) => (tokenString, token, source.startsWith(tokenString)) }
      .filter(_._3)
      .map(t => FoundToken(source.drop(t._1.length), t._2))
      .headOption

  private def findNumberToken(source: String): Option[FoundToken] = {
    val startsWithMinus = source.head == '-'
    val sourceWithoutMinus =
      if (startsWithMinus)
        source.tail
      else
        source
    val numberStringOption = findNumberString(sourceWithoutMinus)
    numberStringOption.map { numberString =>
      val numberStringWithMinus =
        if (startsWithMinus)
          s"-$numberString"
        else
          numberString
      val sourceLeft = source.drop(numberStringWithMinus.length)
      val value = numberStringWithMinus.toDouble
      FoundToken(sourceLeft, NumberToken(value))
    }
  }

  private def findNumberString(source: String): Option[String] =
    if (source.head.isDigit) {
      val integerPartString = source.takeWhile(_.isDigit)
      val restOfSource = source.drop(integerPartString.length)
      if (restOfSource.headOption.contains('.')) {
        val decimalPartString = restOfSource.tail.takeWhile(_.isDigit)
        val numberString = s"$integerPartString.$decimalPartString"
        Some(numberString)
      } else
        Some(integerPartString)
    } else
      None

  private def findStringToken(source: String): Option[FoundToken] =
    if (source.head == '"') {
      val valueOption = findStringValue(source.tail)
      valueOption.map { value =>
        val sourceLeft = source.drop(value.length + 1)
        FoundToken(sourceLeft, StringToken(value))
      }
    } else
      None

  private def findStringValue(source: String): Option[String] = {
    val sourceHead = source.head
    if (source.head == '"')
      None
    else if (source.head == '\\')
      findEscapedCharacter(source.tail).map(c => s"${c.character}${findStringValue(c.sourceLeft).getOrElse("")}")
    else
      Some(s"$sourceHead${findStringValue(source.tail).getOrElse("")}")
  }

  private def findEscapedCharacter(source: String): Option[FoundEscapedCharacter] =
    source.headOption.flatMap { sourceHead =>
      if (sourceHead == '"')
        Some(FoundEscapedCharacter(source.tail, '"'))
      else if (sourceHead == '\\')
        Some(FoundEscapedCharacter(source.tail, '\\'))
      else if (sourceHead == '/')
        Some(FoundEscapedCharacter(source.tail, '/'))
      else if (sourceHead == 'b')
        Some(FoundEscapedCharacter(source.tail, '\b'))
      else if (sourceHead == 'f')
        Some(FoundEscapedCharacter(source.tail, '\f'))
      else if (sourceHead == 'n')
        Some(FoundEscapedCharacter(source.tail, '\n'))
      else if (sourceHead == 'r')
        Some(FoundEscapedCharacter(source.tail, '\r'))
      else if (sourceHead == 't')
        Some(FoundEscapedCharacter(source.tail, '\t'))
      else
        None
    }
}

object JsonTokenizer {
  private lazy val charDefinedTokens: Map[Char, JsonToken] = Map(
    ':' -> ColonToken,
    ',' -> ComaToken,
    '[' -> SquareBracketOpenToken,
    ']' -> SquareBracketCloseToken,
    '{' -> CurlyBracketOpenToken,
    '}' -> CurlyBracketCloseToken
  )

  private lazy val stringDefinedTokens: Map[String, JsonToken] = Map(
    "null" -> NullToken,
    "false" -> FalseToken,
    "true" -> TrueToken
  )

  case class FoundToken(sourceLeft: String, token: JsonToken)

  case class FoundEscapedCharacter(sourceLeft: String, character: Char)

  object FoundToken {
    def startOfSource(source: String): FoundToken =
      FoundToken(source, StartOfSourceToken)

    def endOfSource: FoundToken =
      FoundToken("", EndOfSourceToken)
  }
}