package nullpointer.json.parser

import nullpointer.json.JsonTokens.JsonToken
import nullpointer.json.JsonValues.JsonValue

case class FoundValue(tokensLeft: Stream[JsonToken], value: JsonValue)
