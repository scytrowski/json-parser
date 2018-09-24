package nullpointer.json.tokenizer

import nullpointer.json.testing.{CommonSpec, OptionMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks._

class GeneralCharacterProviderSpec extends CommonSpec with OptionMatchers {
  import GeneralCharacterProviderSpec._

  describe("A GeneralCharacterProvider") {
    it("must return correct character and source left when source head is not reverse solidus and quotation mark") {
      val characterTestCases = Table(
        ("source", "expectedSourceLeft", "expectedCharacter"),
        ("abc", "bc", 'a'),
        ("@ 0bfx", " 0bfx", '@'),
        ("   ", "  ", ' ')
      )
      forAll(characterTestCases) { (source, expectedSourceLeft, expectedCharacter) =>
        val result = GeneralCharacterProvider.provide(source)
        result must beDefined(FoundCharacter(expectedSourceLeft, expectedCharacter))
      }
    }

    it("must return escaped character option when source head is reverse solidus") {
      val escapedCharacterTestCases = Table(
        ("source", "expectedResult"),
        createEscapedCharacterTestCase("\\fabc"),
        createEscapedCharacterTestCase("\\\"def"),
        createEscapedCharacterTestCase("\\m")
      )
      forAll(escapedCharacterTestCases) { (source, expectedResult) =>
        val result = GeneralCharacterProvider.provide(source)
        result mustBe expectedResult
      }
    }

    it("must return end of string character with correct source left when source head is quotation mark") {
      val result = GeneralCharacterProvider.provide("\"abc")
      result must beDefined(FoundCharacter.endOfString("abc"))
    }

    it("must return None when source is empty") {
      val result = GeneralCharacterProvider.provide("")
      result must beEmpty
    }
  }
}

private object GeneralCharacterProviderSpec {
  def createEscapedCharacterTestCase(source: String): (String, Option[FoundCharacter]) =
    source -> EscapedCharacterProvider.provide(source.tail)
}
