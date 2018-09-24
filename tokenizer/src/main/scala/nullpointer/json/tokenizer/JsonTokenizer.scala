package nullpointer.json.tokenizer

import nullpointer.json.JsonTokens.{JsonToken, UnknownToken}

object JsonTokenizer {
  def tokenize(source: String): Stream[JsonToken] =
    Stream.iterate(FoundToken.startOfSource(source)) {
      case FoundToken(sourceLeft, _) =>
        if (!sourceLeft.isEmpty)
          (sourceLeft #:: sourceLeft.tails.toStream)
            .filterNot(_.head.isWhitespace)
            .map(findToken)
            .head
        else
          FoundToken.endOfSource
    }.tail.map(_.token)

  private def findToken(source: String): FoundToken =
    SimpleTokenProvider.provide(source)
      .orElse(NumberTokenProvider.provide(source))
      .orElse(StringTokenProvider.provide(source))
      .getOrElse(createUnknownToken(source))

  private def createUnknownToken(source: String): FoundToken =
    FoundToken(source.tail, UnknownToken(source.head.toString))
}
