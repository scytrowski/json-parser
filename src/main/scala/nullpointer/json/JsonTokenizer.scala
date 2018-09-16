package nullpointer.json

import nullpointer.json.JsonTokens._

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

  object FoundToken {
    def startOfSource(source: String): FoundToken =
      FoundToken(source, StartOfSourceToken)

    def endOfSource: FoundToken =
      FoundToken("", EndOfSourceToken)
  }
}