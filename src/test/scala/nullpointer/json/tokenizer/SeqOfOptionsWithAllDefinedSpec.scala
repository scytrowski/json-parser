package nullpointer.json.tokenizer

import SeqOfOptionsExtensions.SeqOfOptionsWithAllDefined

class SeqOfOptionsWithAllDefinedSpec extends CommonSpec {
  import SeqOfOptionsWithAllDefinedSpec._

  describe("A SeqOfOptionsWithAllDefined") {
    it("must return correct elements when all of elements are defined") {
      val seq: Seq[Option[TestClass]] = Seq(
        Some(TestClass("abc")),
        Some(TestClass("def")),
        Some(TestClass("ghi")),
        Some(TestClass("jkl"))
      )
      val seqAllDefinedOption = seq.allDefined
      seqAllDefinedOption.isDefined mustBe true
      seqAllDefinedOption.get must contain theSameElementsAs Seq(
        TestClass("abc"),
        TestClass("def"),
        TestClass("ghi"),
        TestClass("jkl")
      )
    }

    it("must return None when at least one of elements is None") {
      val seq: Seq[Option[TestClass]] = Seq(
        Some(TestClass("abc")),
        Some(TestClass("def")),
        None,
        Some(TestClass("jkl"))
      )
      seq.allDefined.isDefined mustBe false
    }
  }
}

private object SeqOfOptionsWithAllDefinedSpec {
  case class TestClass(value: String)
}
