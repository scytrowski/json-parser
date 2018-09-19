package nullpointer.json

import nullpointer.json.JsonTokens.JsonToken

trait TokenProvider[T <: JsonToken] {
  def provide(source: String): Option[FoundToken]
}
