package nullpointer.json.tokenizer

import scala.util.{Failure, Success, Try}

private object UnicodeParser {
  def parse(codeString: String): Try[Char] =
    if (codeString.length == 4)
      parseIfHaveCorrectLength(codeString)
    else
      Failure(UnicodeFormatException("Unicode code string must be 4 characters long"))

  private def parseIfHaveCorrectLength(codeString: String): Try[Char] = {
    val codeCharValueOptions = codeString.map(Hexadecimal.valueOf)
    if (codeCharValueOptions.forall(_.isDefined)) {
      val codeCharValues = codeCharValueOptions.map(_.get)
      val code = codeCharValues(3) | (codeCharValues(2) << 4) | (codeCharValues(1) << 8) | (codeCharValues(0) << 12)
      Success(code.toChar)
    } else
      Failure(UnicodeFormatException("Unicode code string must be correct hexadecimal string"))
  }

  case class UnicodeFormatException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
}
