package nullpointer.json

import nullpointer.json.JsonTokens._

import scala.collection.immutable.WrappedString

class JsonTokenizer {
  def tokenize(source: String): Stream[JsonToken] =
    Stream.iterate[(String, JsonToken)]((source, StartOfSourceToken)) {
      case (sourceLeft, _) =>
        if (!sourceLeft.isEmpty)
          (sourceLeft #:: sourceLeft.tails.toStream)
            .map(findToken)
            .dropWhile(_.isEmpty)
            .map(_.get)
            .headOption
            .getOrElse((sourceLeft, EndOfSourceToken))
        else
          (sourceLeft, EndOfSourceToken)
    }.tail.map(_._2)

  private def findToken(source: String): Option[(String, JsonToken)] = {
    val sourceHead = source.head
    if (source.length == 4 && source.take(4) == "null")
      Some(source.drop(4), NullToken)
    else if (source.length == 5 && source.take(5) == "false")
      Some(source.drop(5), FalseToken)
    else if (source.length == 4 && source.take(4) == "true")
      Some(source.drop(4), TrueToken)
    else if (sourceHead == ':')
      Some((source.tail, ColonToken))
    else if (sourceHead == ',')
      Some((source.tail, ComaToken))
    else if (sourceHead == '[')
      Some((source.tail, SquareBracketOpenToken))
    else if (sourceHead == ']')
      Some((source.tail, SquareBracketCloseToken))
    else if (sourceHead == '{')
      Some((source.tail, CurlyBracketOpenToken))
    else if (sourceHead == '}')
      Some((source.tail, CurlyBracketCloseToken))
    else
      None
  }
}
