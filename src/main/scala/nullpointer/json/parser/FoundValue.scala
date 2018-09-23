package nullpointer.json.parser

import nullpointer.json.JsonValues.JsonValue
import nullpointer.json.tokenizer.JsonTokens.JsonToken

case class FoundValue(tokensLeft: Stream[JsonToken], value: JsonValue)
