package nullpointer.json.tokenizer

import nullpointer.json.tokenizer.JsonTokens.NumberToken

private object NumberTokenProvider extends TokenProvider[NumberToken] {
  override def provide(source: String): Option[FoundToken] =
    if (!source.isEmpty) {
      val numberStringOption = getFindNumberStringFunction(source)(source)
      numberStringOption.map { numberString =>
        val sourceLeft = source.drop(numberString.length)
        val numberValue = numberString.toDouble
        FoundToken(sourceLeft, NumberToken(numberValue))
      }
    } else
      None

  private def getFindNumberStringFunction(source: String): String => Option[String] =
    if (source.startsWith("-"))
      findNumberStringWithLeadingMinus
    else
      findNumberString

  private def findNumberStringWithLeadingMinus(source: String): Option[String] = {
    val sourceTail = source.tail
    findNumberString(sourceTail).map(numberString => s"-$numberString")
  }

  private def findNumberString(source: String): Option[String] =
    if (source.head.isDigit) {
      val integerPartString = source.takeWhile(_.isDigit)
      val restOfSource = source.drop(integerPartString.length)
      if (restOfSource.headOption.contains('.')) {
        val decimalPartString = restOfSource.tail.takeWhile(_.isDigit)
        val numberString = s"$integerPartString.$decimalPartString"
        Some(numberString)
      } else
        Some(integerPartString)
    } else
      None
}
