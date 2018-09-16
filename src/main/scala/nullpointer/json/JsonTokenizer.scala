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
    findNull(source)
      .orElse(findFalse(source))
      .orElse(findTrue(source))
      .orElse(findSimpleToken(source))

  private def findNull(source: String): Option[FoundToken] = {
    if (source.length == 4 && source.take(4) == "null")
      Some(FoundToken(source.drop(4), NullToken))
    else
      None
  }

  private def findFalse(source: String): Option[FoundToken] = {
    if (source.length == 5 && source.take(5) == "false")
      Some(FoundToken(source.drop(5), FalseToken))
    else
      None
  }

  private def findTrue(source: String): Option[FoundToken] = {
    if (source.length == 4 && source.take(4) == "true")
      Some(FoundToken(source.drop(4), TrueToken))
    else
      None
  }

  private def findSimpleToken(source: String): Option[FoundToken] = {
    val sourceHead = source.head
    if (sourceHead == ':')
      Some(FoundToken(source.tail, ColonToken))
    else if (sourceHead == ',')
      Some(FoundToken(source.tail, ComaToken))
    else if (sourceHead == '[')
      Some(FoundToken(source.tail, SquareBracketOpenToken))
    else if (sourceHead == ']')
      Some(FoundToken(source.tail, SquareBracketCloseToken))
    else if (sourceHead == '{')
      Some(FoundToken(source.tail, CurlyBracketOpenToken))
    else if (sourceHead == '}')
      Some(FoundToken(source.tail, CurlyBracketCloseToken))
    else
      None
  }
}

object JsonTokenizer {
  case class FoundToken(sourceLeft: String, token: JsonToken)

  object FoundToken {
    def startOfSource(source: String): FoundToken =
      FoundToken(source, StartOfSourceToken)

    def endOfSource: FoundToken =
      FoundToken("", EndOfSourceToken)
  }
}