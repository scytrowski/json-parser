package nullpointer.json.parser

import nullpointer.json.JsonTokens._
import nullpointer.json.JsonValues._
import nullpointer.json.parser.JsonParser.JsonParsingException
import nullpointer.json.testing.{CommonSpec, TryMatchers}

class GeneralTokenEaterSpec extends CommonSpec with TryMatchers {
  describe("A GeneralTokenEater") {
    it("must return JsonNull and correct tokens left when tokens head is NullToken") {
      val result = GeneralTokenEater.eat(Stream(NullToken, TrueToken, FalseToken))
      result must succeedWith(FoundValue(Stream(TrueToken, FalseToken), JsonNull))
    }

    it("must return JsonBoolean(false) when tokens head is FalseToken") {
      val result = GeneralTokenEater.eat(Stream(FalseToken, NullToken, TrueToken))
      result must succeedWith(FoundValue(Stream(NullToken, TrueToken), JsonBoolean(false)))
    }

    it("must return JsonBoolean(true) when tokens head is TrueToken") {
      val result = GeneralTokenEater.eat(Stream(TrueToken, FalseToken, NullToken))
      result must succeedWith(FoundValue(Stream(FalseToken, NullToken), JsonBoolean(true)))
    }

    it("must return JsonNumber with correct value when tokens head is NumberToken") {
      val result = GeneralTokenEater.eat(Stream(NumberToken(5.67912), NullToken, FalseToken))
      result must succeedWith(FoundValue(Stream(NullToken, FalseToken), JsonNumber(5.67912)))
    }

    it("must return array parsing result when tokens head is SquareBracketOpenToken") {
      val tokens = Stream(SquareBracketOpenToken, TrueToken, SquareBracketCloseToken)
      val result = GeneralTokenEater.eat(tokens)
      result mustBe ArrayTokenEater.eat(tokens)
    }

    it("must return object parsing result when tokens head is CurlyBracketOpenToken") {
      val tokens = Stream(CurlyBracketOpenToken, StringToken("abc"), ColonToken, NumberToken(698.012), CurlyBracketCloseToken)
      val result = GeneralTokenEater.eat(tokens)
      result mustBe ObjectTokenEater.eat(tokens)
    }

    it("must return JsonString with correct value when tokens head is StringToken") {
      val result = GeneralTokenEater.eat(Stream(StringToken("zxcv"), TrueToken, NullToken))
      result must succeedWith(FoundValue(Stream(TrueToken, NullToken), JsonString("zxcv")))
    }

    it("must fail with JsonParsingException when tokens input is empty") {
      val result = GeneralTokenEater.eat(Stream.empty)
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when tokens head is ColonToken") {
      val result = GeneralTokenEater.eat(Stream(ColonToken))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when tokens head is ComaToken") {
      val result = GeneralTokenEater.eat(Stream(ComaToken))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when tokens head is SquareBracketCloseToken") {
      val result = GeneralTokenEater.eat(Stream(SquareBracketCloseToken))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when tokens head is CurlyBracketCloseToken") {
      val result = GeneralTokenEater.eat(Stream(CurlyBracketCloseToken))
      result must failWith[JsonParsingException]
    }

    it("must fail with JsonParsingException when tokens head is UnknownToken") {
      val result = GeneralTokenEater.eat(Stream(UnknownToken("abc")))
      result must failWith[JsonParsingException]
    }
  }
}
