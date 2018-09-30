package nullpointer.json.formats

import nullpointer.json.JsonFormatExceptions.JsonDeserializationException
import nullpointer.json.JsonValues.{JsonBoolean, JsonNull, JsonValue}
import nullpointer.json.testing.JsonFormatSpec

class BooleanJsonFormatSpec extends JsonFormatSpec {
  describe("A BooleanJsonFormat") {
    it("must serialize false to JsonBoolean(false)") {
      val result = BooleanJsonFormat.serialize(false)
      result must succeedWith[JsonValue](JsonBoolean(false))
    }

    it("must serialize true to JsonBoolean(true)") {
      val result = BooleanJsonFormat.serialize(true)
      result must succeedWith[JsonValue](JsonBoolean(true))
    }

    it("must deserialize JsonBoolean(false) as false") {
      val result = BooleanJsonFormat.deserialize(JsonBoolean(false))
      result must succeedWith(false)
    }

    it("must deserialize JsonBoolean(true) as true") {
      val result = BooleanJsonFormat.deserialize(JsonBoolean(true))
      result must succeedWith(true)
    }


    it("must fail with JsonDeserializationException when JSON is not JsonBoolean") {
      val result = BooleanJsonFormat.deserialize(JsonNull)
      result must failWith[JsonDeserializationException]
    }
  }
}
