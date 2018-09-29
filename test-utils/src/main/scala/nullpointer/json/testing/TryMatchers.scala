package nullpointer.json.testing

import org.scalatest.MustMatchers
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.reflect.ClassTag
import scala.util.{Failure, Try}

trait TryMatchers {
  import TryMatchers._

  def succeedWith[T](expectedValue: T): TrySucceedWithMatcher[T] = new TrySucceedWithMatcher[T](expectedValue)

  def failWith(expectedThrowable: Throwable): TryFailWithMatcher = new TryFailWithMatcher(expectedThrowable)

  def failWith[E <: Throwable : ClassTag]: TryFailWithTypeMatcher[E] = new TryFailWithTypeMatcher[E]
}

object TryMatchers extends MustMatchers {
  class TrySucceedWithMatcher[T](expectedValue: T) extends Matcher[Try[T]] {
    override def apply(left: Try[T]): MatchResult =
      MatchResult(
        left.isSuccess && left.get === expectedValue,
        s"""Try did not succeeded with "$expectedValue"""",
        s"""Try succeeded with "$expectedValue""""
      )
  }

  class TryFailWithMatcher(expectedThrowable: Throwable) extends Matcher[Try[_]] {
    override def apply(left: Try[_]): MatchResult =
      MatchResult(
        left.isFailure && left.asInstanceOf[Failure[_]].exception === expectedThrowable,
        s"""Try did not failed with "$expectedThrowable"""",
        s"""Try failed with "$expectedThrowable""""
      )
  }

  class TryFailWithTypeMatcher[E <: Throwable](implicit tag: ClassTag[E]) extends Matcher[Try[_]] {
    override def apply(left: Try[_]): MatchResult =
      MatchResult(
        left.isFailure && tag.runtimeClass.isAssignableFrom(left.asInstanceOf[Failure[_]].exception.getClass),
        s"""Try did not failed with "${tag.runtimeClass.getName}"""",
        s"""Try failed with "${tag.runtimeClass.getName}""""
      )
  }
}
