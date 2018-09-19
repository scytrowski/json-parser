package nullpointer.json.tokenizer

import nullpointer.json.tokenizer.JsonTokens.StringToken
import org.scalatest.prop.TableDrivenPropertyChecks._

class StringTokenProviderSpec extends CommonSpec {
  describe("A StringDefinedTokenProvider") {
    it("must return correct StringToken and source left when string on head") {
      val commonStringTestCases = Table(
        ("source", "expectedSourceLeft", "expectedValue"),
        ("\"abcd\"{}[]@", "{}[]@", "abcd"),
        ("\">!%dlk@\"219ab3", "219ab3", ">!%dlk@"),
        ("\"|/$a\"\"abc\"", "\"abc\"", "|/$a")
      )
      forAll(commonStringTestCases) { (source, expectedSourceLeft, expectedValue) =>
        val result = StringTokenProvider.provide(source)
        result.isDefined mustBe true
        result.get mustBe FoundToken(expectedSourceLeft, StringToken(expectedValue))
      }
    }

    it("must return correct StringToken and source left when string with escaped quotation mark on head") {
      val stringWithEscapedQuotationMarkTestCases = Table(
        ("source", "expectedSourceLeft", "expectedValue"),
        ("\"abc\\\"zxc\"def", "def", "abc\"zxc"),
        ("\"\\\"abc\"$@!", "$@!", "\"abc"),
        ("\"zxc\\\"\"poi", "poi", "zxc\""),
        ("\"\\\"mnb\\\"\"abc", "abc", "\"mnb\"")
      )
      forAll(stringWithEscapedQuotationMarkTestCases) { (source, expectedSourceLeft, expectedValue) =>
        val result = StringTokenProvider.provide(source)
        result.isDefined mustBe true
        result.get mustBe FoundToken(expectedSourceLeft, StringToken(expectedValue))
      }
    }

    it("must return correct StringToken and source left when string with escaped reverse solidus on head") {
      val stringWithEscapedReverseSolidusTestCases = Table(
        ("source", "expectedSourceLeft", "expectedValue"),
        ("\"abc\\\\zxc\"def", "def", "abc\\zxc"),
        ("\"\\\\abc\"$@!", "$@!", "\\abc"),
        ("\"zxc\\\\\"poi", "poi", "zxc\\"),
        ("\"\\\\mnb\\\\\"abc", "abc", "\\mnb\\")
      )
      forAll(stringWithEscapedReverseSolidusTestCases) { (source, expectedSourceLeft, expectedValue) =>
        val result = StringTokenProvider.provide(source)
        result.isDefined mustBe true
        result.get mustBe FoundToken(expectedSourceLeft, StringToken(expectedValue))
      }
    }

    it("must return correct StringToken and source left when string with escaped solidus on head") {
      val stringWithEscapedSolidusTestCases = Table(
        ("source", "expectedSourceLeft", "expectedValue"),
        ("\"abc\\/zxc\"def", "def", "abc/zxc"),
        ("\"\\/abc\"$@!", "$@!", "/abc"),
        ("\"zxc\\/\"poi", "poi", "zxc/"),
        ("\"\\/mnb\\/\"abc", "abc", "/mnb/")
      )
      forAll(stringWithEscapedSolidusTestCases) { (source, expectedSourceLeft, expectedValue) =>
        val result = StringTokenProvider.provide(source)
        result.isDefined mustBe true
        result.get mustBe FoundToken(expectedSourceLeft, StringToken(expectedValue))
      }
    }

    it("must return correct StringToken and source left when string with escaped backspace on head") {
      val stringWithEscapedBackspaceTestCases = Table(
        ("source", "expectedSourceLeft", "expectedValue"),
        ("\"abc\\bzxc\"def", "def", "abc\bzxc"),
        ("\"\\babc\"$@!", "$@!", "\babc"),
        ("\"zxc\\b\"poi", "poi", "zxc\b"),
        ("\"\\bmnb\\b\"abc", "abc", "\bmnb\b")
      )
      forAll(stringWithEscapedBackspaceTestCases) { (source, expectedSourceLeft, expectedValue) =>
        val result = StringTokenProvider.provide(source)
        result.isDefined mustBe true
        result.get mustBe FoundToken(expectedSourceLeft, StringToken(expectedValue))
      }
    }

    it("must return correct StringToken and source left when string with escaped formfeed on head") {
      val stringWithEscapedFormfeedTestCases = Table(
        ("source", "expectedSourceLeft", "expectedValue"),
        ("\"abc\\fzxc\"def", "def", "abc\fzxc"),
        ("\"\\fabc\"$@!", "$@!", "\fabc"),
        ("\"zxc\\f\"poi", "poi", "zxc\f"),
        ("\"\\fmnb\\f\"abc", "abc", "\fmnb\f")
      )
      forAll(stringWithEscapedFormfeedTestCases) { (source, expectedSourceLeft, expectedValue) =>
        val result = StringTokenProvider.provide(source)
        result.isDefined mustBe true
        result.get mustBe FoundToken(expectedSourceLeft, StringToken(expectedValue))
      }
    }

    it("must return correct StringToken and source left when string with escaped new line on head") {
      val stringWithEscapedNewLineTestCases = Table(
        ("source", "expectedSourceLeft", "expectedValue"),
        ("\"abc\\nzxc\"def", "def", "abc\nzxc"),
        ("\"\\nabc\"$@!", "$@!", "\nabc"),
        ("\"zxc\\n\"poi", "poi", "zxc\n"),
        ("\"\\nmnb\\n\"abc", "abc", "\nmnb\n")
      )
      forAll(stringWithEscapedNewLineTestCases) { (source, expectedSourceLeft, expectedValue) =>
        val result = StringTokenProvider.provide(source)
        result.isDefined mustBe true
        result.get mustBe FoundToken(expectedSourceLeft, StringToken(expectedValue))
      }
    }

    it("must return correct StringToken and source left when string with escaped carriage return on head") {
      val stringWithEscapedCarriageReturnTestCases = Table(
        ("source", "expectedSourceLeft", "expectedValue"),
        ("\"abc\\rzxc\"def", "def", "abc\rzxc"),
        ("\"\\rabc\"$@!", "$@!", "\rabc"),
        ("\"zxc\\r\"poi", "poi", "zxc\r"),
        ("\"\\rmnb\\r\"abc", "abc", "\rmnb\r")
      )
      forAll(stringWithEscapedCarriageReturnTestCases) { (source, expectedSourceLeft, expectedValue) =>
        val result = StringTokenProvider.provide(source)
        result.isDefined mustBe true
        result.get mustBe FoundToken(expectedSourceLeft, StringToken(expectedValue))
      }
    }

    it("must return correct StringToken and source left when string with escaped horizontal tab on head") {
      val stringWithEscapedHorizontalTabTestCases = Table(
        ("source", "expectedSourceLeft", "expectedValue"),
        ("\"abc\\tzxc\"def", "def", "abc\tzxc"),
        ("\"\\tabc\"$@!", "$@!", "\tabc"),
        ("\"zxc\\t\"poi", "poi", "zxc\t"),
        ("\"\\tmnb\\t\"abc", "abc", "\tmnb\t")
      )
      forAll(stringWithEscapedHorizontalTabTestCases) { (source, expectedSourceLeft, expectedValue) =>
        val result = StringTokenProvider.provide(source)
        result.isDefined mustBe true
        result.get mustBe FoundToken(expectedSourceLeft, StringToken(expectedValue))
      }
    }

    it("must return correct StringToken and source left when string with escaped unicode characters on head") {
      val stringWithEscapedUnicodeCharactersTestCases = Table(
        ("source", "expectedSourceLeft", "expectedValue"),
        ("\"abc\\u071azxc\"def", "def", s"abc${1818.toChar}zxc"),
        ("\"\\u5df0abc\"$@!", "$@!", s"${24048.toChar}abc"),
        ("\"zxc\\uf07e\"poi", "poi", s"zxc${61566.toChar}"),
        ("\"\\u7f0amnb\\u2ba9\"abc", "abc", s"${32522.toChar}mnb${11177.toChar}")
      )
      forAll(stringWithEscapedUnicodeCharactersTestCases) { (source, expectedSourceLeft, expectedValue) =>
        val result = StringTokenProvider.provide(source)
        result.isDefined mustBe true
        result.get mustBe FoundToken(expectedSourceLeft, StringToken(expectedValue))
      }
    }

    it("must return None when string is not on head") {
      val result = StringTokenProvider.provide("abc")
      result.isEmpty mustBe true
    }

    it("must return None when string is not terminated by quotation mark") {
      val result = StringTokenProvider.provide("\"abc")
      result.isEmpty mustBe true
    }

    it("must return None when source is empty") {
      val result = StringTokenProvider.provide("")
      result.isEmpty mustBe true
    }
  }
}
