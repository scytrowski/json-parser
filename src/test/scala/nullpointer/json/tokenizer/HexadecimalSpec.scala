package nullpointer.json.tokenizer

import org.scalatest.prop.TableDrivenPropertyChecks._

class HexadecimalSpec extends CommonSpec {
  import HexadecimalSpec._

  describe("Hexadecimal") {
    val validHexadecimalCharCases = Table(
      ("validHexadecimalChar", "expectedValue"),
      validHexadecimalChars.toSeq:_*
    )
    forAll(validHexadecimalCharCases) { (validHexadecimalChar, expectedValue) =>
      it(s"must return $expectedValue when char is '$validHexadecimalChar'") {
        val result = Hexadecimal.valueOf(validHexadecimalChar)
        result.isDefined mustBe true
        result.get mustBe expectedValue
      }
    }

    it("must return None when char is not valid hexadecimal character") {
      val invalidHexadecimalCharCases = Table(
        "invalidHexadecimalChar",
        invalidHexadecimalChars: _*
      )
      forAll(invalidHexadecimalCharCases) { invalidHexadecimalChar =>
        val result = Hexadecimal.valueOf(invalidHexadecimalChar)
        result.isEmpty mustBe true
      }
    }
  }
}

private object HexadecimalSpec {
  private lazy val validHexadecimalChars: Map[Char, Byte] = Map(
    '0' -> 0,
    '1' -> 1,
    '2' -> 2,
    '3' -> 3,
    '4' -> 4,
    '5' -> 5,
    '6' -> 6,
    '7' -> 7,
    '8' -> 8,
    '9' -> 9,
    'a' -> 10,
    'A' -> 10,
    'b' -> 11,
    'B' -> 11,
    'c' -> 12,
    'C' -> 12,
    'd' -> 13,
    'D' -> 13,
    'e' -> 14,
    'E' -> 14,
    'f' -> 15,
    'F' -> 15
  )

  private lazy val invalidHexadecimalChars: Seq[Char] = (0 until 256)
    .map(_.toChar)
    .diff(validHexadecimalChars.keys.toSeq)
}
