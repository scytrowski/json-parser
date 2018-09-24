package nullpointer.json.tokenizer

import nullpointer.json.testing.CommonSpec
import nullpointer.json.JsonTokens._

class JsonTokenizerSpec extends CommonSpec {
  describe("A JsonTokenizer") {
    it("must emit NullToken when null on head") {
      val tokens = JsonTokenizer.tokenize("null")
      tokens.head mustBe NullToken
    }

    it("must emit FalseToken when false on head") {
      val tokens = JsonTokenizer.tokenize("false")
      tokens.head mustBe FalseToken
    }

    it("must emit TrueToken when true on head") {
      val tokens = JsonTokenizer.tokenize("true")
      tokens.head mustBe TrueToken
    }

    it("must emit NumberToken(0) when 0 on head") {
      val tokens = JsonTokenizer.tokenize("0")
      tokens.head mustBe NumberToken(0)
    }

    it("must emit NumberToken with correct value when negative integer on head") {
      val tokens = JsonTokenizer.tokenize("-68927")
      tokens.head mustBe NumberToken(-68927)
    }

    it("must emit NumberToken with correct value when positive integer on head") {
      val tokens = JsonTokenizer.tokenize("10945")
      tokens.head mustBe NumberToken(10945)
    }

    it("must emit NumberToken with correct value when negative decimal on head") {
      val tokens = JsonTokenizer.tokenize("-781.605")
      tokens.head mustBe NumberToken(-781.605)
    }

    it("must emit NumberToken with correct value when positive decimal on head") {
      val tokens = JsonTokenizer.tokenize("39.708")
      tokens.head mustBe NumberToken(39.708)
    }

    it("must emit StringToken with correct value when string on head") {
      val tokens = JsonTokenizer.tokenize("\"zxcvbnm\"")
      tokens.head mustBe StringToken("zxcvbnm")
    }

    it("must emit StringToken with correct value when string with escaped quotation marks on head") {
      val tokens = JsonTokenizer.tokenize("\"zxc\\\"vb\\\"nm\"")
      tokens.head mustBe StringToken("zxc\"vb\"nm")
    }

    it("must emit StringToken with correct value when string with escaped reverse solidus on head") {
      val tokens = JsonTokenizer.tokenize("\"zxc\\\\vb\\\\nm\"")
      tokens.head mustBe StringToken("zxc\\vb\\nm")
    }

    it("must emit StringToken with correct value when string with escaped solidus on head") {
      val tokens = JsonTokenizer.tokenize("\"zxc\\/vb\\/nm\"")
      tokens.head mustBe StringToken("zxc/vb/nm")
    }

    it("must emit StringToken with correct value when string with escaped backspace on head") {
      val tokens = JsonTokenizer.tokenize("\"zxc\\bvb\\bnm\"")
      tokens.head mustBe StringToken(s"zxc\bvb\bnm")
    }

    it("must emit StringToken with correct value when string with escaped formfeed on head") {
      val tokens = JsonTokenizer.tokenize("\"zxc\\fvb\\fnm\"")
      tokens.head mustBe StringToken(s"zxc\fvb\fnm")
    }

    it("must emit StringToken with correct value when string with escaped newline on head") {
      val tokens = JsonTokenizer.tokenize("\"zxc\\nvb\\nnm\"")
      tokens.head mustBe StringToken("zxc\nvb\nnm")
    }

    it("must emit StringToken with correct value when string with escaped carriage return on head") {
      val tokens = JsonTokenizer.tokenize("\"zxc\\rvb\\rnm\"")
      tokens.head mustBe StringToken("zxc\rvb\rnm")
    }

    it("must emit StringToken with correct value when string with escaped horizontal tab on head") {
      val tokens = JsonTokenizer.tokenize("\"zxc\\tvb\\tnm\"")
      tokens.head mustBe StringToken("zxc\tvb\tnm")
    }

    it("must emit StringToken with correct value when string with escaped unicode characters on head") {
      val tokens = JsonTokenizer.tokenize("\"zxc\\u69cfvb\\u0a1dnm\"")
      tokens.head mustBe StringToken(s"zxc${27087.toChar}vb${2589.toChar}nm")
    }

    it("must emit ColonToken when : on head") {
      val tokens = JsonTokenizer.tokenize(":")
      tokens.head mustBe ColonToken
    }

    it("must emit ComaToken when , on head") {
      val tokens = JsonTokenizer.tokenize(",")
      tokens.head mustBe ComaToken
    }

    it("must emit SquareBracketOpenToken when [ on head") {
      val tokens = JsonTokenizer.tokenize("[")
      tokens.head mustBe SquareBracketOpenToken
    }

    it("must emit SquareBracketCloseToken when ] on head") {
      val tokens = JsonTokenizer.tokenize("]")
      tokens.head mustBe SquareBracketCloseToken
    }

    it("must emit CurlyBracketOpenToken when { on head") {
      val tokens = JsonTokenizer.tokenize("{")
      tokens.head mustBe CurlyBracketOpenToken
    }

    it("must emit CurlyBracketCloseToken when } on head") {
      val tokens = JsonTokenizer.tokenize("}")
      tokens.head mustBe CurlyBracketCloseToken
    }

    it("must emit EndOfSourceToken on empty source") {
      val tokens = JsonTokenizer.tokenize("")
      tokens.head mustBe EndOfSourceToken
    }

    it("must emit EndOfSourceToken after last parsed token") {
      val tokens = JsonTokenizer.tokenize("{}")
      tokens.drop(2).head mustBe EndOfSourceToken
    }

    it("must emit EndOfSourceToken after emitted EndOfSourceToken") {
      val tokens = JsonTokenizer.tokenize("")
      tokens.tail.head mustBe EndOfSourceToken
    }

    it("must emit UnknownToken when unable to recognize character on head") {
      val tokens = JsonTokenizer.tokenize("a")
      tokens.head mustBe UnknownToken("a")
    }
  }
}
