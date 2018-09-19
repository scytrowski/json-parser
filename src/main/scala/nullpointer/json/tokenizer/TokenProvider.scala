package nullpointer.json.tokenizer

import nullpointer.json.tokenizer.JsonTokens.JsonToken

trait TokenProvider[T <: JsonToken] {
  def provide(source: String): Option[FoundToken]
}
