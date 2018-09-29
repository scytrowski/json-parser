package nullpointer.json.testing

import nullpointer.json.testing.TryMatchers.TryFailWithMatcher

import scala.util.{Failure, Success}

class TryFailWithMatcherSpec extends CommonSpec {
  describe("A TryFailWithMatcher") {
    it("must match when failed with expected throwable") {
      val expectedThrowable = new IllegalStateException("Something gone wrong")
      val matcher = new TryFailWithMatcher(expectedThrowable)
      val result = matcher(Failure(expectedThrowable))
      result.matches mustBe true
    }

    it("must not match when try is success") {
      val expectedThrowable = new IllegalStateException("Something gone wrong")
      val matcher = new TryFailWithMatcher(expectedThrowable)
      val result = matcher(Success("abc"))
      result.matches mustBe false
    }

    it("must not match when failed with throwable other than expected") {
      val expectedThrowable = new IllegalStateException("Something gone wrong")
      val matcher = new TryFailWithMatcher(expectedThrowable)
      val actualThrowable = new IllegalAccessError("Something other gone wrong")
      val result = matcher(Failure(actualThrowable))
      result.matches mustBe false
    }
  }
}
