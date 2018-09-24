package nullpointer.json.testing

import nullpointer.json.testing.TryMatchers.TrySucceedWithMatcher

import scala.util.{Failure, Success}

class TrySucceedWithMatcherSpec extends CommonSpec {
  describe("A TrySucceedWithMatcher") {
    it("must when try is success and has correct value") {
      val matcher = new TrySucceedWithMatcher[String]("abc")
      val result = matcher(Success("abc"))
      result.matches mustBe true
    }

    it("must not match when try is failure") {
      val matcher = new TrySucceedWithMatcher[String]("abc")
      val result = matcher(Failure(new IllegalStateException("Some exception")))
      result.matches mustBe false
    }

    it("must not match when result is different than expected") {
      val matcher = new TrySucceedWithMatcher[String]("abc")
      val result = matcher(Success("def"))
      result.matches mustBe false
    }
  }
}
