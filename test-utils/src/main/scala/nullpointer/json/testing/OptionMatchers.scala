package nullpointer.json.testing

import org.scalatest.MustMatchers
import org.scalatest.matchers.{MatchResult, Matcher}

trait OptionMatchers {
  import OptionMatchers._

  def beDefined[T](expectedValue: T): OptionBeDefinedMatcher[T] = new OptionBeDefinedMatcher[T](expectedValue)

  def beEmpty: OptionBeEmptyMatcher = new OptionBeEmptyMatcher
}

object OptionMatchers extends MustMatchers {
  class OptionBeDefinedMatcher[T](expectedValue: T) extends Matcher[Option[T]] {
    override def apply(left: Option[T]): MatchResult =
      MatchResult(
        left.isDefined && left.get === expectedValue,
        s"""Option is not defined with value "$expectedValue"""",
        s"""Option is defined with value "$expectedValue""""
      )
  }

  class OptionBeEmptyMatcher extends Matcher[Option[_]] {
    override def apply(left: Option[_]): MatchResult =
      MatchResult(
        left.isEmpty,
        "Option is not empty",
        "Option is empty"
      )
  }
}
