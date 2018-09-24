package nullpointer.json.tokenizer

import nullpointer.json.testing.CommonSpec
import nullpointer.json.JsonTokens._
import org.scalatest.prop.TableDrivenPropertyChecks._

class SimpleTokenProviderSpec extends CommonSpec {
  describe("A SimpleTokenProvider") {
    val definedStrings = Table(
      ("source", "expectedSourceLeft", "expectedToken"),
      (":zxc", "zxc", ColonToken),
      (",abc", "abc", ComaToken),
      ("[def", "def", SquareBracketOpenToken),
      ("]ghi", "ghi", SquareBracketCloseToken),
      ("{!@#", "!@#", CurlyBracketOpenToken),
      ("}$%^", "$%^", CurlyBracketCloseToken),
      ("null|abc", "|abc", NullToken),
      ("false&()", "&()", FalseToken),
      ("true'//", "'//", TrueToken)
    )
    forAll(definedStrings) { (string, expectedSourceLeft, expectedToken) =>
      it(s"must return $expectedToken when head starts with $string") {
        val result = SimpleTokenProvider.provide(string)
        result.isDefined mustBe true
        result.get mustBe FoundToken(expectedSourceLeft, expectedToken)
      }
    }

    it("must return None when head starts with undefined string") {
      val undefinedStrings = Table(
        "string",
        "abcd",
        "#$@!",
        ".;olkm",
        "126509"
      )
      forAll(undefinedStrings) { undefinedString =>
        val result = SimpleTokenProvider.provide(undefinedString)
        result.isEmpty mustBe true
      }
    }
  }
}
