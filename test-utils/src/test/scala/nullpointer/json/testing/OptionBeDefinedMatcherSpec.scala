package nullpointer.json.testing

import nullpointer.json.testing.OptionMatchers.OptionBeDefinedMatcher

class OptionBeDefinedMatcherSpec extends CommonSpec {
  describe("An OptionBeDefinedMatcher") {
    it("must match when option is defined with correct value") {
      val matcher = new OptionBeDefinedMatcher("abc")
      val result = matcher(Some("abc"))
      result.matches mustBe true
    }

    it("must not match when option is empty") {
      val matcher = new OptionBeDefinedMatcher("abc")
      val result = matcher(None)
      result.matches mustBe false
    }

    it("must not match when option is defined with value other than expected") {
      val matcher = new OptionBeDefinedMatcher("abc")
      val result = matcher(Some("def"))
      result.matches mustBe false
    }
  }
}
