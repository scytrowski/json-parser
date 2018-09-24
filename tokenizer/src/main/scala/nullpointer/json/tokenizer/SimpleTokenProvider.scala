package nullpointer.json.tokenizer

import nullpointer.json.JsonTokens._

private object SimpleTokenProvider extends TokenProvider[JsonToken] {
  private lazy val definedTokens: Map[String, JsonToken] = Map(
    ":" -> ColonToken,
    "," -> ComaToken,
    "[" -> SquareBracketOpenToken,
    "]" -> SquareBracketCloseToken,
    "{" -> CurlyBracketOpenToken,
    "}" -> CurlyBracketCloseToken,
    "null" -> NullToken,
    "false" -> FalseToken,
    "true" -> TrueToken
  )

  override def provide(source: String): Option[FoundToken] =
    definedTokens
      .find { case (tokenString, _) => source.startsWith(tokenString) }
      .map { case (tokenString, token) => FoundToken(source.drop(tokenString.length), token) }
}
