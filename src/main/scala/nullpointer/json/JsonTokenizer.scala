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
    findCharToken(source)
      .orElse(findStringToken(source))

  private def findCharToken(source: String): Option[FoundToken] =
    charTokens
      .get(source.head)
      .map(t => FoundToken(source.tail, t))

  private def findStringToken(source: String): Option[FoundToken] =
    stringTokens
      .view
      .map { case (tokenString, token) => (tokenString, token, source.startsWith(tokenString)) }
      .filter(_._3)
      .map(t => FoundToken(source.drop(t._1.length), t._2))
      .headOption
}

object JsonTokenizer {
  private lazy val charTokens: Map[Char, JsonToken] = Map(
    ':' -> ColonToken,
    ',' -> ComaToken,
    '[' -> SquareBracketOpenToken,
    ']' -> SquareBracketCloseToken,
    '{' -> CurlyBracketOpenToken,
    '}' -> CurlyBracketCloseToken
  )

  private lazy val stringTokens: Map[String, JsonToken] = Map(
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