package nullpointer.json

import nullpointer.json.JsonTokens.NumberToken

object NumberTokenProvider extends TokenProvider[NumberToken] {
  override def provide(source: String): Option[FoundToken] =
    if (!source.isEmpty) {
      val startsWithMinus = source.head == '-'
      val sourceWithoutMinus =
        if (startsWithMinus)
          source.tail
        else
          source
      val numberStringOption = findNumberString(sourceWithoutMinus)
      numberStringOption.map { numberString =>
        val numberStringWithMinus =
          if (startsWithMinus)
            s"-$numberString"
          else
            numberString
        val sourceLeft = source.drop(numberStringWithMinus.length)
        val value = numberStringWithMinus.toDouble
        FoundToken(sourceLeft, NumberToken(value))
      }
    } else
      None

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
