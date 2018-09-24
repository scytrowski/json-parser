package nullpointer.json.parser

import nullpointer.json.parser.JsonParser.JsonParsingException
import nullpointer.json.testing.CommonSpec
import nullpointer.json.JsonTokens._
import nullpointer.json.JsonValues._
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.util.Failure

class ArrayTokenEaterSpec extends CommonSpec {
  describe("An ArrayTokenEater") {
    it("must return empty array with correct tokens left when SquareBracketOpenToken followed by SquareBracketCloseToken on head") {
      val result = ArrayTokenEater.eat(Stream(
        SquareBracketOpenToken,
        SquareBracketCloseToken,
        NullToken,
        FalseToken))
      result.isSuccess mustBe true
      result.get mustBe FoundValue(Stream(NullToken, FalseToken), JsonArray())
    }

    it("must return array with correct single element and correct tokens left when tokens contain array with simple token") {
      val arrayWithSingleElementCases = Table(
        ("tokens", "expectedTokensLeft", "expectedValue"),
        (Stream(
          SquareBracketOpenToken,
            NullToken,
          SquareBracketCloseToken,
          NullToken, FalseToken), Seq(NullToken, FalseToken), JsonArray(JsonNull)),
        (Stream(
          SquareBracketOpenToken,
            FalseToken,
          SquareBracketCloseToken,
          TrueToken, NullToken), Seq(TrueToken, NullToken), JsonArray(JsonBoolean(false))),
        (Stream(
          SquareBracketOpenToken,
            TrueToken,
          SquareBracketCloseToken,
          StringToken("abc"), NullToken), Seq(StringToken("abc"), NullToken), JsonArray(JsonBoolean(true))),
        (Stream(
          SquareBracketOpenToken,
            NumberToken(24.567),
          SquareBracketCloseToken,
          NumberToken(19.321), FalseToken), Seq(NumberToken(19.321), FalseToken), JsonArray(JsonNumber(24.567))),
        (Stream(
          SquareBracketOpenToken,
            StringToken("zxcv"),
          SquareBracketCloseToken,
          FalseToken, TrueToken), Seq(FalseToken, TrueToken), JsonArray(JsonString("zxcv")))
      )
      forAll(arrayWithSingleElementCases) { (tokens, expectedTokensLeft, expectedValue) =>
        val result = ArrayTokenEater.eat(tokens)
        result.isSuccess mustBe true
        result.get mustBe FoundValue(expectedTokensLeft.toStream, expectedValue)
      }
    }

    it("must return array with correct multiple elements and correct tokens left when tokens contain array with multiple tokens") {
      val arrayWithMultipleElementCases = Table(
        ("tokens", "expectedTokensLeft", "expectedValue"),
        (Stream(
          SquareBracketOpenToken,
              FalseToken,
            ComaToken,
              NullToken,
          SquareBracketCloseToken,
          NullToken, FalseToken
        ), Seq(NullToken, FalseToken), JsonArray(JsonBoolean(false), JsonNull)),
        (Stream(
          SquareBracketOpenToken,
              StringToken("abcd"),
            ComaToken,
              NumberToken(12345),
            ComaToken,
              NullToken,
          SquareBracketCloseToken,
          TrueToken, StringToken("abc")
        ), Seq(TrueToken, StringToken("abc")), JsonArray(JsonString("abcd"), JsonNumber(12345), JsonNull)),
        (Stream(
          SquareBracketOpenToken,
              NumberToken(0.12345),
            ComaToken,
              StringToken("zxcvf"),
            ComaToken,
              FalseToken,
          SquareBracketCloseToken,
          NumberToken(76.6891), NullToken
        ), Seq(NumberToken(76.6891), NullToken), JsonArray(JsonNumber(0.12345), JsonString("zxcvf"), JsonBoolean(false)))
      )
      forAll(arrayWithMultipleElementCases) { (tokens, expectedTokensLeft, expectedValue) =>
        val result = ArrayTokenEater.eat(tokens)
        result.isSuccess mustBe true
        result.get mustBe FoundValue(expectedTokensLeft.toStream, expectedValue)
      }
    }

    it("must return array with correct tokens left when tokens contain array with nested array") {
      val result = ArrayTokenEater.eat(Stream(
        SquareBracketOpenToken,
          SquareBracketOpenToken,
            FalseToken,
          SquareBracketCloseToken,
        SquareBracketCloseToken,
        TrueToken, NullToken
      ))
      result.isSuccess mustBe true
      result.get mustBe FoundValue(Stream(TrueToken, NullToken), JsonArray(JsonArray(JsonBoolean(false))))
    }

    it("must return array with correct tokens left when tokens contain array with object") {
      val result = ArrayTokenEater.eat(Stream(
        SquareBracketOpenToken,
          CurlyBracketOpenToken,
              StringToken("abc"),
            ColonToken,
              NumberToken(1234),
          CurlyBracketCloseToken,
        SquareBracketCloseToken,
        NullToken, FalseToken
      ))
      result.isSuccess mustBe true
      result.get mustBe FoundValue(Stream(NullToken, FalseToken), JsonArray(JsonObject("abc" -> JsonNumber(1234))))
    }

    it("must fail with JsonParsingException when tokens input is empty") {
      val result = ArrayTokenEater.eat(Stream.empty)
      result.isFailure mustBe true
      result.asInstanceOf[Failure[FoundValue]].exception.isInstanceOf[JsonParsingException] mustBe true
    }

    it("must fail with JsonParsingException when tokens head is not SquareBracketOpenToken") {
      val result = ArrayTokenEater.eat(Stream(NullToken))
      result.isFailure mustBe true
      result.asInstanceOf[Failure[FoundValue]].exception.isInstanceOf[JsonParsingException] mustBe true
    }

    it("must fail with JsonParsingException when tokens contain array of elements not separated by ComaToken") {
      val result = ArrayTokenEater.eat(Stream(
        SquareBracketOpenToken,
          FalseToken,
          NullToken,
        SquareBracketCloseToken
      ))
      result.isFailure mustBe true
      result.asInstanceOf[Failure[FoundValue]].exception.isInstanceOf[JsonParsingException] mustBe true
    }

    it("must fail with JsonParsingException when array tokens sequence does not end with SquareBracketCloseToken") {
      val result = ArrayTokenEater.eat(Stream(SquareBracketOpenToken))
      result.isFailure mustBe true
      result.asInstanceOf[Failure[FoundValue]].exception.isInstanceOf[JsonParsingException] mustBe true
    }
  }
}
