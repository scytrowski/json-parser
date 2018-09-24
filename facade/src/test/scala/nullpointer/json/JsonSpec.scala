package nullpointer.json

import nullpointer.json.testing.{CommonSpec, TryMatchers}
import nullpointer.json.JsonValues._

class JsonSpec extends CommonSpec with TryMatchers {
  describe("Json") {
    it("must parse null") {
      val result = Json.parse("null")
      result must succeedWith[JsonValue](JsonNull)
    }

    it("must parse boolean false") {
      val result = Json.parse("false")
      result must succeedWith[JsonValue](JsonBoolean(false))
    }

    it("must parse boolean true") {
      val result = Json.parse("true")
      result must succeedWith[JsonValue](JsonBoolean(true))
    }

    it("must parse zero") {
      val result = Json.parse("0")
      result must succeedWith[JsonValue](JsonNumber(0))
    }

    it("must parse negative integer") {
      val result = Json.parse("-579")
      result must succeedWith[JsonValue](JsonNumber(-579))
    }

    it("must parse positive integer") {
      val result = Json.parse("791")
      result must succeedWith[JsonValue](JsonNumber(791))
    }

    it("must parse negative decimal") {
      val result = Json.parse("-105.69213")
      result must succeedWith[JsonValue](JsonNumber(-105.69213))
    }

    it("must parse positive decimal") {
      val result = Json.parse("4056.791")
      result must succeedWith[JsonValue](JsonNumber(4056.791))
    }

    it("must parse empty string") {
      val result = Json.parse("\"\"")
      result must succeedWith[JsonValue](JsonString(""))
    }

    it("must parse string") {
      val result = Json.parse("\"abcdefghij\"")
      result must succeedWith[JsonValue](JsonString("abcdefghij"))
    }

    it("must parse string with escaped quotation mark") {
      val result = Json.parse("\"abc\\\"def\"")
      result must succeedWith[JsonValue](JsonString("abc\"def"))
    }

    it("must parse string with escaped reverse solidus") {
      val result = Json.parse("\"abc\\\\def\"")
      result must succeedWith[JsonValue](JsonString("abc\\def"))
    }

    it("must parse string with escaped solidus") {
      val result = Json.parse("\"abc\\/def\"")
      result must succeedWith[JsonValue](JsonString("abc/def"))
    }

    it("must parse string with escaped backspace") {
      val result = Json.parse("\"abc\\bdef\"")
      result must succeedWith[JsonValue](JsonString(s"abc${8.toChar}def"))
    }

    it("must parse string with escaped formfeed") {
      val result = Json.parse("\"abc\\fdef\"")
      result must succeedWith[JsonValue](JsonString(s"abc${12.toChar}def"))
    }

    it("must parse string with escaped newline") {
      val result = Json.parse("\"abc\\ndef\"")
      result must succeedWith[JsonValue](JsonString("abc\ndef"))
    }

    it("must parse string with escaped carriage return") {
      val result = Json.parse("\"abc\\rdef\"")
      result must succeedWith[JsonValue](JsonString("abc\rdef"))
    }

    it("must parse string with escaped unicode character") {
      val result = Json.parse("\"abc\\u3a9fdef")
      result must succeedWith[JsonValue](JsonString(s"abc${15007.toChar}def"))
    }

    it("must parse string with escaped horizontal tab") {
      val result = Json.parse("\"abc\\tdef\"")
      result must succeedWith[JsonValue](JsonString("abc\tdef"))
    }

    it("must parse empty array") {
      val result = Json.parse("[]")
      result must succeedWith[JsonValue](JsonArray(List.empty))
    }

    it("must parse array") {
      val resultTry = Json.parse("[null, true, -5.9871, \"abc\"]")
      resultTry.isSuccess mustBe true
      val result = resultTry.get
      result.isInstanceOf[JsonArray] mustBe true
      val resultArray = result.asInstanceOf[JsonArray]
      resultArray.elements must contain theSameElementsAs Seq(
        JsonNull,
        JsonBoolean(true),
        JsonNumber(-5.9871),
        JsonString("abc")
      )
    }

    it("must parse empty object") {
      val result = Json.parse("{}")
      result must succeedWith[JsonValue](JsonObject())
    }

    it("must parse object") {
      val resultTry = Json.parse("{ \"a\": null, \"b\": true, \"c\": -5.9871, \"d\": \"abc\" }")
      resultTry.isSuccess mustBe true
      val result = resultTry.get
      result.isInstanceOf[JsonObject] mustBe true
      val resultObject = result.asInstanceOf[JsonObject]
      resultObject.elements must contain theSameElementsAs Map(
        "a" -> JsonNull,
        "b" -> JsonBoolean(true),
        "c" -> JsonNumber(-5.9871),
        "d" -> JsonString("abc")
      )
    }
  }
}
