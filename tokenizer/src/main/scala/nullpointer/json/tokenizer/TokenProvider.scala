package nullpointer.json.tokenizer

import nullpointer.json.JsonTokens.JsonToken

private trait TokenProvider[T <: JsonToken] {
  def provide(source: String): Option[FoundToken]
}
