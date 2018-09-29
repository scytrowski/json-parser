package nullpointer.json.testing.extensions

import nullpointer.json.testing.CommonSpec
import nullpointer.json.testing.extensions.FloatExtensions.FloatWithIsDefined
import nullpointer.json.testing.random.RandomDataProvider

import org.scalatest.prop.TableDrivenPropertyChecks._

class FloatWithIsDefinedSpec extends CommonSpec {
  import FloatWithIsDefinedSpec._

  describe("A FloatWithIsDefined") {
    it("must be defined if is correct number within float values range") {
      val definedNumberTestCases = Table(
        "number",
        Floats:_*
      )
      forAll(definedNumberTestCases) { number =>
        createFloatWithIsDefined(number).isDefined mustBe true
      }
    }

    it("must not be defined if is NaN") {
      createFloatWithIsDefined(Float.NaN).isDefined mustBe false
    }

    it("must not be defined if is +inf") {
      createFloatWithIsDefined(Float.PositiveInfinity).isDefined mustBe false
    }

    it("must not be defined is if -inf") {
      createFloatWithIsDefined(Float.NegativeInfinity).isDefined mustBe false
    }
  }
}

private object FloatWithIsDefinedSpec {
  private lazy val triesPerTest = 100

  lazy val Floats: Seq[Float] =
    RandomDataProvider
      .provideFloats
      .filter(isDefinedFloat)
      .distinct
      .take(triesPerTest)
      .toList

  def createFloatWithIsDefined(float: Float): FloatWithIsDefined = new FloatWithIsDefined(float)

  private def isDefinedFloat(float: Float): Boolean =
    !float.isNaN && !float.isInfinite
}
