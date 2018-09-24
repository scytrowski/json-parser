package nullpointer.json.tokenizer

import nullpointer.json.JsonTokens.JsonToken

object JsonTokenizer {
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
    SimpleTokenProvider.provide(source)
      .orElse(NumberTokenProvider.provide(source))
      .orElse(StringTokenProvider.provide(source))
}
