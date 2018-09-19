package nullpointer.json.tokenizer

import nullpointer.json.tokenizer.JsonTokens.JsonToken

private trait TokenProvider[T <: JsonToken] {
  def provide(source: String): Option[FoundToken]
}
