package nullpointer.json.tokenizer

import nullpointer.json.tokenizer.JsonTokens.{EndOfSourceToken, JsonToken, StartOfSourceToken}

case class FoundToken(sourceLeft: String, token: JsonToken)

object FoundToken {
  def startOfSource(source: String): FoundToken = FoundToken(source, StartOfSourceToken)

  def endOfSource: FoundToken = FoundToken("", EndOfSourceToken)
}
