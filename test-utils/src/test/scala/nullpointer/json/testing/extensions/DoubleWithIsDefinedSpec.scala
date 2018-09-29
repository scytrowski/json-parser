package nullpointer.json.testing.extensions

import nullpointer.json.testing.CommonSpec
import nullpointer.json.testing.extensions.DoubleExtensions.DoubleWithIsDefined
import nullpointer.json.testing.random.RandomDataProvider

import org.scalatest.prop.TableDrivenPropertyChecks._

class DoubleWithIsDefinedSpec extends CommonSpec {
  import DoubleWithIsDefinedSpec._

  describe("A DoubleWithIsDefined") {
    it("must be defined if is correct number within double values range") {
      val definedNumberTestCases = Table(
        "number",
        Doubles:_*
      )
      forAll(definedNumberTestCases) { number =>
        createDoubleWithIsDefined(number).isDefined mustBe true
      }
    }

    it("must not be defined if is NaN") {
      createDoubleWithIsDefined(Double.NaN).isDefined mustBe false
    }

    it("must not be defined if is +inf") {
      createDoubleWithIsDefined(Double.PositiveInfinity).isDefined mustBe false
    }

    it("must not be defined if is -inf") {
      createDoubleWithIsDefined(Double.NegativeInfinity).isDefined mustBe false
    }
  }
}

private object DoubleWithIsDefinedSpec {
  private lazy val triesPerTest = 100

  lazy val Doubles: Seq[Double] =
    RandomDataProvider
      .provideDoubles
      .filter(isDefinedDouble)
      .distinct
      .take(triesPerTest)
      .toList

  def createDoubleWithIsDefined(double: Double): DoubleWithIsDefined = new DoubleWithIsDefined(double)

  private def isDefinedDouble(double: Double): Boolean =
    !double.isNaN && !double.isInfinite
}
