package nullpointer.json.tokenizer

import nullpointer.json.testing.{CommonSpec, OptionMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks._

class UnicodeCharacterProviderSpec extends CommonSpec with OptionMatchers {
  import UnicodeCharacterProviderSpec._

  describe("An UnicodeCharacterProvider") {
    it("must return correct unicode character and source tail when correct unicode code string is on head") {
      val correctCodeStringTestCases = Table(
        ("source", "expectedSourceLeft", "expectedCharacter"),
        createCorrectCodeStringTestCase("ab70"),
        createCorrectCodeStringTestCase("9fa1"),
        createCorrectCodeStringTestCase("2715"),
        createCorrectCodeStringTestCase("0000"),
        createCorrectCodeStringTestCase("ffff")
      )
      forAll(correctCodeStringTestCases) { (source, expectedSourceLeft, expectedCharacter) =>
        val result = UnicodeCharacterProvider.provide(source)
        result must beDefined(FoundCharacter(expectedSourceLeft, expectedCharacter))
      }
    }

    it("must return None when incorrect unicode code string is on head") {
      val incorrectCodeStringTestCases = Table(
        "source",
        "g1a9",
        "f%2a",
        "8cr1",
        "fcap"
      )
      forAll(incorrectCodeStringTestCases) { source =>
        val result = UnicodeCharacterProvider.provide(source)
        result must beEmpty
      }
    }

    it("must return None when source length is less than 4") {
      val sourceLengthLessThan4TestCases = Table(
        "source",
        "a",
        "ab",
        "abc"
      )
      forAll(sourceLengthLessThan4TestCases) { source =>
        val result = UnicodeCharacterProvider.provide(source)
        result must beEmpty
      }
    }

    it("must return None when source is empty") {
      val result = UnicodeCharacterProvider.provide("")
      result must beEmpty
    }
  }
}

private object UnicodeCharacterProviderSpec {
  def createCorrectCodeStringTestCase(source: String): (String, String, Char) = {
    val codeString = source.take(4)
    val expectedSourceLeft = source.drop(4)
    val expectedCharacter = UnicodeParser.parse(codeString).get
    (source, expectedSourceLeft, expectedCharacter)
  }
}
