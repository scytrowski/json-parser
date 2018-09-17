package nullpointer.json

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object UnicodeParser {
  private lazy val hexadecimalRegex: Regex = "^[0-9a-fA-F]+$".r

  def parse(codeString: String): Try[Char] =
    if (codeString.length == 4)
      parseIfHaveCorrectLength(codeString)
    else
      Failure(UnicodeFormatException("Unicode code string must be 4 characters long"))

  private def parseIfHaveCorrectLength(codeString: String): Try[Char] =
    if (hexadecimalRegex.pattern.matcher(codeString).matches) {
      val codeCharValues = codeString.map(getHexValue)
      val code = codeCharValues(3) | (codeCharValues(2) << 4) | (codeCharValues(1) << 8) | (codeCharValues(0) << 12)
      Success(code.toChar)
    } else
      Failure(UnicodeFormatException("Unicode code string must be correct hexadecimal string"))

  private def getHexValue(hexChar: Char): Byte = hexChar match {
    case '0' => 0
    case '1' => 1
    case '2' => 2
    case '3' => 3
    case '4' => 4
    case '5' => 5
    case '6' => 6
    case '7' => 7
    case '8' => 8
    case '9' => 9
    case 'a' | 'A' => 10
    case 'b' | 'B' => 11
    case 'c' | 'C' => 12
    case 'd' | 'D' => 13
    case 'e' | 'E' => 14
    case 'f' | 'F' => 15
  }

  case class UnicodeFormatException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
}
