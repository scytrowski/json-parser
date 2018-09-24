package nullpointer.json.tokenizer

import nullpointer.json.JsonTokens.{EndOfSourceToken, JsonToken, StartOfSourceToken}

private case class FoundToken(sourceLeft: String, token: JsonToken)

private object FoundToken {
  def startOfSource(source: String): FoundToken = FoundToken(source, StartOfSourceToken)

  def endOfSource: FoundToken = FoundToken("", EndOfSourceToken)
}
