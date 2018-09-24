package nullpointer.json.tokenizer

import nullpointer.json.testing.CommonSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

class EscapedCharacterProviderSpec extends CommonSpec {
  import EscapedCharacterProviderSpec._

  describe("An EscapedCharacterProvider") {
    val charactersThatCanBeEscapedTestCases = Table(
      ("char", "expectedResultChar"),
      charactersThatCanBeEscaped.toSeq:_*
    )
    forAll(charactersThatCanBeEscapedTestCases) { (char, expectedResultChar) =>
      it(s"must return correct character and source tail when source head is $char") {
        val source = s"${char}abc"
        val result = EscapedCharacterProvider.provide(source)
        result.isDefined mustBe true
        result.get mustBe FoundCharacter(source.tail, expectedResultChar)
      }
    }

    it("must return unicode character option when source head is u") {
      val unicodeCharacterTestCases = Table(
        ("source", "expectedResult"),
        createUnicodeCharacterTestCase("u91af"),
        createUnicodeCharacterTestCase("uabcd"),
        createUnicodeCharacterTestCase("u123")
      )
      forAll(unicodeCharacterTestCases) { (source, expectedResult) =>
        val result = EscapedCharacterProvider.provide(source)
        result mustBe expectedResult
      }
    }

    it("must return None when source head cannot be escaped") {
      val charactersThatCannotBeEscapedTestCases = Table(
        "char",
        charactersThatCannotBeEscaped:_*
      )
      forAll(charactersThatCannotBeEscapedTestCases) { char =>
        val source = char.toString
        val result = EscapedCharacterProvider.provide(source)
        result.isEmpty mustBe true
      }
    }

    it("must return None when source is empty") {
      val result = EscapedCharacterProvider.provide("")
      result.isEmpty mustBe true
    }
  }
}

private object EscapedCharacterProviderSpec {
  lazy val charactersThatCanBeEscaped: Map[Char, Char] = Map(
    '"' -> '"',
    '\\' -> '\\',
    '/' -> '/',
    'b' -> '\b',
    'f' -> '\f',
    'n' -> '\n',
    'r' -> '\r',
    't' -> '\t'
  )

  lazy val charactersThatCannotBeEscaped: Seq[Char] =
    (0 until 256).map(_.toChar)
      .diff(charactersThatCanBeEscaped.keys.toSeq :+ 'u')

  def createUnicodeCharacterTestCase(source: String): (String, Option[FoundCharacter]) =
    source -> UnicodeCharacterProvider.provide(source.tail)
}
