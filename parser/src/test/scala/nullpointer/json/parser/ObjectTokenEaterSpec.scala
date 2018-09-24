package nullpointer.json.parser

import nullpointer.json.JsonTokens._
import nullpointer.json.JsonValues._
import nullpointer.json.parser.JsonParser.JsonParsingException
import nullpointer.json.testing.{CommonSpec, TryMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.util.Failure

class ObjectTokenEaterSpec extends CommonSpec with TryMatchers {
  describe("An ObjectTokenEater") {
    it("must return empty object and correct tokens left when CurlyBracketOpenToken followed by CurlyBracketCloseToken on head") {
      val result = ObjectTokenEater.eat(Stream(CurlyBracketOpenToken, CurlyBracketCloseToken, NullToken, TrueToken))
      result must succeedWith(FoundValue(Stream(NullToken, TrueToken), JsonObject()))
    }

    it("must return object with correct single element and correct tokens left when tokens contain object with key and simple token") {
      val objectWithSingleElementCases = Table(
        ("tokens", "expectedTokensLeft", "expectedValue"),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("abc"), ColonToken, NullToken,
          CurlyBracketCloseToken,
          NullToken, FalseToken), Seq(NullToken, FalseToken), JsonObject("abc" -> JsonNull)),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("abc"), ColonToken, FalseToken,
          CurlyBracketCloseToken,
          TrueToken, NullToken), Seq(TrueToken, NullToken), JsonObject("abc" -> JsonBoolean(false))),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("abc"), ColonToken, TrueToken,
          CurlyBracketCloseToken,
          NumberToken(63.091), FalseToken), Seq(NumberToken(63.091), FalseToken), JsonObject("abc" -> JsonBoolean(true))),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("abc"), ColonToken, NumberToken(49.727),
          CurlyBracketCloseToken,
          StringToken("zxc"), NullToken), Seq(StringToken("zxc"), NullToken), JsonObject("abc" -> JsonNumber(49.727))),
        (Stream(
          CurlyBracketOpenToken,
            StringToken("abc"), ColonToken, StringToken("def"),
          CurlyBracketCloseToken,
          FalseToken, NumberToken(9.317)), Seq(FalseToken, NumberToken(9.317)), JsonObject("abc" -> JsonString("def")))
      )
      forAll(objectWithSingleElementCases) { (tokens, expectedTokensLeft, expectedValue) =>
        val result = ObjectTokenEater.eat(tokens)
        result must succeedWith(FoundValue(expectedTokensLeft.toStream, expectedValue))
      }
    }

    it("must return object with correct multiple elements and correct tokens left when tokens contain object with multiple tokens") {
      val objectWithMultipleElementCases = Table(
        ("tokens", "expectedTokensLeft", "expectedValue"),
        (Stream(
          CurlyBracketOpenToken,
              StringToken("abc"), ColonToken, FalseToken,
            ComaToken,
              StringToken("def"), ColonToken, NullToken,
          CurlyBracketCloseToken,
          NullToken, FalseToken
        ), Seq(NullToken, FalseToken), JsonObject("abc" -> JsonBoolean(false), "def" -> JsonNull)),
        (Stream(
          CurlyBracketOpenToken,
              StringToken("abc"), ColonToken, StringToken("abcd"),
            ComaToken,
              StringToken("def"), ColonToken, NumberToken(12345),
            ComaToken,
              StringToken("ghi"), ColonToken, NullToken,
          CurlyBracketCloseToken,
          TrueToken, StringToken("abc")
        ), Seq(TrueToken, StringToken("abc")), JsonObject("abc" -> JsonString("abcd"), "def" -> JsonNumber(12345), "ghi" -> JsonNull)),
        (Stream(
          CurlyBracketOpenToken,
              StringToken("abc"), ColonToken, NumberToken(-7.891),
            ComaToken,
              StringToken("def"), ColonToken, StringToken("zxcvf"),
            ComaToken,
              StringToken("ghi"), ColonToken, FalseToken,
          CurlyBracketCloseToken,
          NumberToken(391.034), NullToken
        ), Seq(NumberToken(391.034), NullToken), JsonObject("abc" -> JsonNumber(-7.891), "def" -> JsonString("zxcvf"), "ghi" -> JsonBoolean(false)))
      )
      forAll(objectWithMultipleElementCases) { (tokens, expecteddTokensLeft, expectedValue) =>
        val result = ObjectTokenEater.eat(tokens)
        result must succeedWith(FoundValue(expecteddTokensLeft.toStream, expectedValue))
      }
    }

    it("must return object with correct tokens left when tokens contain object with nested object") {
      val result = ObjectTokenEater.eat(Stream(
        CurlyBracketOpenToken,
          StringToken("abc"), ColonToken,
            CurlyBracketOpenToken,
              StringToken("def"), ColonToken, NullToken,
            CurlyBracketCloseToken,
        CurlyBracketCloseToken,
        TrueToken, NullToken
      ))
      result must succeedWith(FoundValue(Stream(TrueToken, NullToken), JsonObject("abc" -> JsonObject("def" -> JsonNull))))
    }

    it("must return object with correct tokens left when tokens contain object with array") {
      val result = ObjectTokenEater.eat(Stream(
        CurlyBracketOpenToken,
          StringToken("abc"), ColonToken,
            SquareBracketOpenToken,
              NullToken,
            SquareBracketCloseToken,
        CurlyBracketCloseToken,
        NullToken, FalseToken
      ))
      result must succeedWith(FoundValue(Stream(NullToken, FalseToken), JsonObject("abc" -> JsonArray(JsonNull))))
    }

    it("must fail with JsonParsingException when tokens input is empty") {
      val result = ObjectTokenEater.eat(Stream.empty)
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when tokens head is not CurlyBracketOpenToken") {
      val result = ObjectTokenEater.eat(Stream(NullToken))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when tokens contain object of elements with not separated key and value by ColonToken") {
      val result = ObjectTokenEater.eat(Stream(
        CurlyBracketOpenToken,
          StringToken("abc"), NullToken,
        CurlyBracketCloseToken
      ))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when tokens contain object of elements not separated by ComaToken") {
      val result = ObjectTokenEater.eat(Stream(
        CurlyBracketOpenToken,
          StringToken("abc"), NullToken,
          StringToken("def"), FalseToken,
        CurlyBracketCloseToken
      ))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when object tokens sequence does not end with CurlyBracketCloseToken") {
      val result = ObjectTokenEater.eat(Stream(CurlyBracketOpenToken))
      result must failWith[JsonParsingException]
    }
  }
}
