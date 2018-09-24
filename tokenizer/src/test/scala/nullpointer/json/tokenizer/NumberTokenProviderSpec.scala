package nullpointer.json.tokenizer

import nullpointer.json.testing.{CommonSpec, OptionMatchers}
import nullpointer.json.JsonTokens.NumberToken
import org.scalatest.prop.TableDrivenPropertyChecks._

class NumberTokenProviderSpec extends CommonSpec with OptionMatchers {
  describe("A NumberTokenProvider") {
    it("must return correct number and source left when source starts with 0") {
      val result = NumberTokenProvider.provide("0abc")
      result must beDefined(FoundToken("abc", NumberToken(0)))
    }

    it("must return correct number and source left when source starts with integer") {
      val integerTestCases = Table(
        ("source", "expectedValue"),
        (-100 to 100).map(i => i.toString -> i):_*
      )
      forAll(integerTestCases) { (source, expectedValue) =>
        val result = NumberTokenProvider.provide(source)
        result must beDefined(FoundToken("", NumberToken(expectedValue)))
      }
    }

    it("must return correct number and source left when source starts with decimal") {
      val decimalTestCases = Table(
        ("source", "expectedValue"),
        (-1000 to 1000).map(i => i * 50.0 / 1000.0).map(v => v.toString -> v):_*
      )
      forAll(decimalTestCases) { (source, expectedValue) =>
        val result = NumberTokenProvider.provide(source)
        result must beDefined(FoundToken("", NumberToken(expectedValue)))
      }
    }

    it("must return None when number is not on head") {
      val result = NumberTokenProvider.provide("abc")
      result must beEmpty
    }

    it("must return None when source is empty") {
      val result = NumberTokenProvider.provide("")
      result must beEmpty
    }
  }
}
