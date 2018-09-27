package nullpointer.json.parser

import nullpointer.json.JsonTokens._
import nullpointer.json.JsonValues._
import nullpointer.json.parser.JsonParser.JsonParsingException
import nullpointer.json.testing.{CommonSpec, TryMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.util.Failure

class JsonParserSpec extends CommonSpec with TryMatchers {
  describe("A JsonParser") {
    it("must return JsonNull when NullToken on head") {
      val result = JsonParser.parse(Stream(NullToken))
      result must succeedWith[JsonValue](JsonNull)
    }

    it("must return JsonBoolean(false) when FalseToken on head") {
      val result = JsonParser.parse(Stream(FalseToken))
      result must succeedWith[JsonValue](JsonBoolean(false))
    }

    it("must return JsonBoolean(true) when TrueToken on head") {
      val result = JsonParser.parse(Stream(TrueToken))
      result must succeedWith[JsonValue](JsonBoolean(true))
    }

    it("must return JsonNumber with correct value when NumberToken on head") {
      val result = JsonParser.parse(Stream(NumberToken(-967.1335)))
      result must succeedWith[JsonValue](JsonNumber(-967.1335))
    }

    it("must return JsonString with correct value when StringToken on head") {
      val result = JsonParser.parse(Stream(StringToken("alpo399v,>134!^3!lo")))
      result must succeedWith[JsonValue](JsonString("alpo399v,>134!^3!lo"))
    }

    it("must return empty array when SquareBracketOpenToken followed by SquareBracketCloseToken on head") {
      val result = JsonParser.parse(Stream(
        SquareBracketOpenToken,
        SquareBracketCloseToken
      ))
      result must succeedWith[JsonValue](JsonArray())
    }

    it("must return correct array when SquareBracketOpenToken followed by single element token followed by SquareBracketCloseToken on head") {
      val arrayWithSingleElementCases = Table(
        ("tokenSequence", "expectedValue"),
        (Stream(SquareBracketOpenToken,
                  NullToken,
                SquareBracketCloseToken), JsonArray(JsonNull)),
        (Stream(SquareBracketOpenToken,
                  FalseToken,
                SquareBracketCloseToken), JsonArray(JsonBoolean(false))),
        (Stream(SquareBracketOpenToken,
                  TrueToken,
                SquareBracketCloseToken), JsonArray(JsonBoolean(true))),
        (Stream(SquareBracketOpenToken,
                  NumberToken(87.157),
                SquareBracketCloseToken), JsonArray(JsonNumber(87.157))),
        (Stream(SquareBracketOpenToken,
                  StringToken("zxcv"),
                SquareBracketCloseToken), JsonArray(JsonString("zxcv")))
      )
      forAll(arrayWithSingleElementCases) { (tokenSequence, expectedValue) =>
        val result = JsonParser.parse(tokenSequence)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must return correct array when SquareBracketOpenToken followed by multiple element tokens separated by ComaToken followed by SquareBracketCloseToken on head") {
      val arrayWithMultipleElementCases = Table(
        ("tokenSequence", "expectedValue"),
        (Stream(
          SquareBracketOpenToken,
            NullToken, ComaToken,
            TrueToken,
          SquareBracketCloseToken), JsonArray(JsonNull, JsonBoolean(true))),
        (Stream(
          SquareBracketOpenToken,
            FalseToken, ComaToken,
            NumberToken(4.901), ComaToken,
            NullToken,
          SquareBracketCloseToken), JsonArray(JsonBoolean(false), JsonNumber(4.901), JsonNull)),
        (Stream(
          SquareBracketOpenToken,
            StringToken("lkop"), ComaToken,
            TrueToken, ComaToken,
            NumberToken(36.791), ComaToken,
            NullToken,
          SquareBracketCloseToken), JsonArray(JsonString("lkop"), JsonBoolean(true), JsonNumber(36.791), JsonNull))
      )
      forAll(arrayWithMultipleElementCases) { (tokenSequence, expectedValue) =>
        val result = JsonParser.parse(tokenSequence)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must return correct array when contain nested array") {
      val result = JsonParser.parse(Stream(
        SquareBracketOpenToken,
          SquareBracketOpenToken,
            TrueToken, ComaToken,
            NumberToken(-15.712), ComaToken,
          SquareBracketCloseToken,
        SquareBracketCloseToken))
      result must succeedWith[JsonValue](JsonArray(JsonArray(JsonBoolean(true), JsonNumber(-15.712))))
    }

    it("must return correct array when contain object") {
      val result = JsonParser.parse(Stream(
        SquareBracketOpenToken,
          CurlyBracketOpenToken,
            StringToken("abc"), ColonToken, StringToken("def"),
          CurlyBracketCloseToken,
        SquareBracketCloseToken
      ))
      result must succeedWith[JsonValue](JsonArray(JsonObject(
        "abc" -> JsonString("def")
      )))
    }

    it("must fail with JsonParsingException when array is not terminated by SquareBracketCloseToken") {
      val result = JsonParser.parse(Stream(
        SquareBracketOpenToken,
          NullToken
      ))
      result must failWith[JsonParsingException]
    }

    it("must return empty object when CurlyBracketOpenToken followed by CurlyBracketCloseToken on head") {
      val result = JsonParser.parse(Stream(
        CurlyBracketOpenToken,
        CurlyBracketCloseToken
      ))
      result must succeedWith[JsonValue](JsonObject())
    }

    it("must return correct object when CurlyBracketOpenToken followed by StringToken, ColonToken and single element token followed by CurlyBracketCloseToken on head") {
      val objectWithSingleElementCases = Table(
        ("tokenSequence", "expectedValue"),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("abc"), ColonToken, NullToken,
          CurlyBracketCloseToken
        ), JsonObject("abc" -> JsonNull)),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("def"), ColonToken, FalseToken,
          CurlyBracketCloseToken
        ), JsonObject("def" -> JsonBoolean(false))),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("ghi"), ColonToken, TrueToken,
          CurlyBracketCloseToken
        ), JsonObject("ghi" -> JsonBoolean(true))),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("jkl"), ColonToken, NumberToken(63.087),
          CurlyBracketCloseToken
        ), JsonObject("jkl" -> JsonNumber(63.087))),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("mno"), ColonToken, StringToken("uiop"),
          CurlyBracketCloseToken
        ), JsonObject("mno" -> JsonString("uiop")))
      )
      forAll(objectWithSingleElementCases) { (tokenSequence, expectedValue) =>
        val result = JsonParser.parse(tokenSequence)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must return correct object when CurlyBracketOpenToken followed by (StringToken, ColonToken, single element token, ComaToken)+ followed by CurlyBracketCloseToken on head") {
      val objectWithMultipleElementCases = Table(
        ("tokenSequence", "expectedValue"),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("abc"), ColonToken, TrueToken, ComaToken,
            StringToken("def"), ColonToken, NumberToken(53.907),
          CurlyBracketCloseToken
        ), JsonObject(
          "abc" -> JsonBoolean(true),
          "def" -> JsonNumber(53.907))),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("abc"), ColonToken, NullToken, ComaToken,
            StringToken("def"), ColonToken, StringToken("ytr"), ComaToken,
            StringToken("ghi"), ColonToken, FalseToken,
          CurlyBracketCloseToken
        ), JsonObject(
          "abc" -> JsonNull,
          "def" -> JsonString("ytr"),
          "ghi" -> JsonBoolean(false))),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("abc"), ColonToken, NumberToken(-72.651), ComaToken,
            StringToken("def"), ColonToken, TrueToken, ComaToken,
            StringToken("ghi"), ColonToken, StringToken("fgh"), ComaToken,
            StringToken("jkl"), ColonToken, NullToken, ComaToken,
          CurlyBracketCloseToken
        ), JsonObject(
          "abc" -> JsonNumber(-72.651),
          "def" -> JsonBoolean(true),
          "ghi" -> JsonString("fgh"),
          "jkl" -> JsonNull
        ))
      )
      forAll(objectWithMultipleElementCases) { (tokenSequence, expectedValue) =>
        val result = JsonParser.parse(tokenSequence)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must return correct object when contain nested object") {
      val result = JsonParser.parse(Stream(
        CurlyBracketOpenToken,
          StringToken("abc"), ColonToken,
          CurlyBracketOpenToken,
            StringToken("def"), ColonToken, NumberToken(1337),
          CurlyBracketCloseToken,
        CurlyBracketCloseToken
      ))
      result must succeedWith[JsonValue](JsonObject(
        "abc" -> JsonObject(
          "def" -> JsonNumber(1337)
        )
      ))
    }

    it("must return correct object when contain array") {
      val result = JsonParser.parse(Stream(
        CurlyBracketOpenToken,
          StringToken("abc"), ColonToken,
          SquareBracketOpenToken,
            NumberToken(69.013),
          SquareBracketCloseToken,
        CurlyBracketCloseToken
      ))
      result must succeedWith[JsonValue](JsonObject(
        "abc" -> JsonArray(JsonNumber(69.013))
      ))
    }

    it("must fail with JsonParsingException when object tokens with elements not separated by comas on head") {
      val result = JsonParser.parse(Stream(
        CurlyBracketOpenToken,
          StringToken("abc"), ColonToken, NullToken,
          StringToken("def"), ColonToken, FalseToken,
        CurlyBracketCloseToken
      ))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when object contains element without key") {
      val result = JsonParser.parse(Stream(
        CurlyBracketOpenToken,
          ColonToken, NullToken,
        CurlyBracketCloseToken
      ))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when object is not terminated by CurlyBracketCloseToken") {
      val result = JsonParser.parse(Stream(
        CurlyBracketOpenToken,
          StringToken("abc"), ColonToken, NullToken
      ))
      result.isFailure mustBe true
      result.asInstanceOf[Failure[JsonValue]].exception.isInstanceOf[JsonParsingException] mustBe true
    }

    it("must fail with JsonParsingException when ColonToken on head") {
      val result = JsonParser.parse(Stream(ColonToken))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when ComaToken on head") {
      val result = JsonParser.parse(Stream(ComaToken))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when SquareBracketCloseToken on head") {
      val result = JsonParser.parse(Stream(SquareBracketCloseToken))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when CurlyBracketCloseToken on head") {
      val result = JsonParser.parse(Stream(CurlyBracketCloseToken))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when UnknownToken on head") {
      val result = JsonParser.parse(Stream(UnknownToken("abc")))
      result must failWith[JsonParsingException]
    }
  }
}
