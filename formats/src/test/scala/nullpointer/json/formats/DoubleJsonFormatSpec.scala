package nullpointer.json.formats

import nullpointer.json.JsonValues._
import nullpointer.json.formats.JsonFormatExceptions.JsonDeserializationException
import nullpointer.json.testing.JsonFormatSpec
import nullpointer.json.testing.random.RandomDataProvider
import org.scalatest.prop.TableDrivenPropertyChecks._

class DoubleJsonFormatSpec extends JsonFormatSpec {
  import DoubleJsonFormatSpec._

  describe("A DoubleJsonFormat") {
    it("must serialize to JsonNumber with correct value") {
      val serializeDoubleTestCases = Table(
        ("number", "expectedValue"),
        Doubles.map(n => n -> JsonNumber(n)):_*
      )
      forAll(serializeDoubleTestCases) { (number, expectedValue) =>
        val result = DoubleJsonFormat.serialize(number)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must deserialize JsonNumber to correct Double") {
      val deserializeJsonNumberTestCases = Table(
        ("json", "expectedNumber"),
        Doubles.map(n => JsonNumber(n) -> n):_*
      )
      forAll(deserializeJsonNumberTestCases) { (json, expectedNumber) =>
        val result = DoubleJsonFormat.deserialize(json)
        result must succeedWith(expectedNumber)
      }
    }

    it("must fail with JsonDeserializationException on deserialization when JSON is not JsonNumber") {
      val failingDeserializationTestCases = Table(
        "json",
        JsonNull,
        JsonBoolean(false),
        JsonBoolean(true),
        JsonString("abc"),
        JsonArray(JsonBoolean(true), JsonNull),
        JsonObject("a" -> JsonNull, "b" -> JsonBoolean(false))
      )
      forAll(failingDeserializationTestCases) { json =>
        val result = DoubleJsonFormat.deserialize(json)
        result must failWith[JsonDeserializationException]
      }
    }
  }
}

private object DoubleJsonFormatSpec {
  private lazy val triesPerTest = 100

  lazy val Doubles: Seq[Double] =
    RandomDataProvider
      .provideDoubles
      .distinct
      .take(triesPerTest)
      .toList
}
