package nullpointer.json

object JsonTokens {
  sealed trait JsonToken

  case object NullToken extends JsonToken
  case object FalseToken extends JsonToken
  case object TrueToken extends JsonToken
  case class NumberToken(value: Double) extends JsonToken
  case class StringToken(value: String) extends JsonToken

  case object ColonToken extends JsonToken
  case object ComaToken extends JsonToken
  case object SquareBracketOpenToken extends JsonToken
  case object SquareBracketCloseToken extends JsonToken
  case object CurlyBracketOpenToken extends JsonToken
  case object CurlyBracketCloseToken extends JsonToken

  case object StartOfSourceToken extends JsonToken
  case object EndOfSourceToken extends JsonToken
}
