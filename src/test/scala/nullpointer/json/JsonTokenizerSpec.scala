package nullpointer.json

import nullpointer.json.JsonTokens._

class JsonTokenizerSpec extends CommonSpec {
  describe("A JsonTokenizer") {
    it("must emit NullToken when null on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("null")
      tokens.head mustBe NullToken
    }

    it("must emit FalseToken when false on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("false")
      tokens.head mustBe FalseToken
    }

    it("must emit TrueToken when true on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("true")
      tokens.head mustBe TrueToken
    }

    it("must emit NumberToken(0) when 0 on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("0")
      tokens.head mustBe NumberToken(0)
    }

    it("must emit NumberToken with correct value when negative integer on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("-68927")
      tokens.head mustBe NumberToken(-68927)
    }

    it("must emit NumberToken with correct value when positive integer on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("10945")
      tokens.head mustBe NumberToken(10945)
    }

    it("must emit NumberToken with correct value when negative decimal on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("-781.605")
      tokens.head mustBe NumberToken(-781.605)
    }

    it("must emit NumberToken with correct value when positive decimal on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("39.708")
      tokens.head mustBe NumberToken(39.708)
    }

    it("must emit StringToken with correct value when string on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("\"zxcvbnm\"")
      tokens.head mustBe StringToken("zxcvbnm")
    }

    it("must emit StringToken with correct value when string with escaped quotation marks on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("\"zxc\\\"vb\\\"nm\"")
      tokens.head mustBe StringToken("zxc\"vb\"nm")
    }

    it("must emit StringToken with correct value when string with escaped reverse solidus on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("\"zxc\\\\vb\\\\nm\"")
      tokens.head mustBe StringToken("zxc\\vb\\nm")
    }

    it("must emit StringToken with correct value when string with escaped solidus on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("\"zxc\\/vb\\/nm\"")
      tokens.head mustBe StringToken("zxc/vb/nm")
    }

    it("must emit StringToken with correct value when string with escaped backspace on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("\"zxc\\bvb\\bnm\"")
      tokens.head mustBe StringToken(s"zxc${8.toChar}vb${8.toChar}nm")
    }

    it("must emit StringToken with correct value when string with escaped formfeed on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("\"zxc\\fvb\\fnm\"")
      tokens.head mustBe StringToken(s"zxc${12.toChar}vb${8.toChar}nm")
    }

    it("must emit StringToken with correct value when string with escaped newline on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("\"zxc\\nvb\\nnm\"")
      tokens.head mustBe StringToken("zxc\nvb\nnm")
    }

    it("must emit StringToken with correct value when string with escaped carriage return on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("\"zxc\\rvb\\rnm\"")
      tokens.head mustBe StringToken("zxc\rvb\rnm")
    }

    it("must emit StringToken with correct value when string with escaped horizontal tab on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("\"zxc\\tvb\\tnm\"")
      tokens.head mustBe StringToken("zxc\tvb\tnm")
    }

    it("must emit StringToken with correct value when string with escaped unicode characters on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("\"zxc\\u69cfvb\\u0a1dnm\"")
      tokens.head mustBe StringToken(s"zxc${27087.toChar}vb${2589.toChar}nm")
    }

    it("must emit ColonToken when : on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize(":")
      tokens.head mustBe ColonToken
    }

    it("must emit ComaToken when , on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize(",")
      tokens.head mustBe ComaToken
    }

    it("must emit SquareBracketOpenToken when [ on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("[")
      tokens.head mustBe SquareBracketOpenToken
    }

    it("must emit SquareBracketCloseToken when ] on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("]")
      tokens.head mustBe SquareBracketCloseToken
    }

    it("must emit CurlyBracketOpenToken when { on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("{")
      tokens.head mustBe CurlyBracketOpenToken
    }

    it("must emit CurlyBracketCloseToken when } on head") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("}")
      tokens.head mustBe CurlyBracketCloseToken
    }

    it("must emit EndOfSourceToken on empty source") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("")
      tokens.head mustBe EndOfSourceToken
    }

    it("must emit EndOfSourceToken after last parsed token") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("{}")
      tokens.drop(2).head mustBe EndOfSourceToken
    }

    it("must emit EndOfSourceToken after emitted EndOfSourceToken") {
      val tokenizer = new JsonTokenizer
      val tokens = tokenizer.tokenize("")
      tokens.tail.head mustBe EndOfSourceToken
    }
  }
}
