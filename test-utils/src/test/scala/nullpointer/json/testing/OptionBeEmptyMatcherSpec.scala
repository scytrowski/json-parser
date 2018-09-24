package nullpointer.json.testing

import nullpointer.json.testing.OptionMatchers.OptionBeEmptyMatcher

class OptionBeEmptyMatcherSpec extends CommonSpec {
  describe("An OptionBeEmptyMatcher") {
    it("must match when option is empty") {
      val matcher = new OptionBeEmptyMatcher
      val result = matcher(None)
      result.matches mustBe true
    }

    it("must not match when option is defined") {
      val matcher = new OptionBeEmptyMatcher
      val result = matcher(Some("abc"))
      result.matches mustBe false
    }
  }
}
