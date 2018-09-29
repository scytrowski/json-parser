package nullpointer.json.formats

import nullpointer.json.JsonValues._
import nullpointer.json.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}
import nullpointer.json.testing.JsonFormatSpec
import nullpointer.json.testing.random.RandomDataProvider
import nullpointer.json.testing.extensions.DoubleExtensions
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

    it("must fail with JsonSerializationException when try to serialize NaN") {
      val result = DoubleJsonFormat.serialize(Double.NaN)
      result must failWith[JsonSerializationException]
    }

    it("must fail with JsonSerializationException when try to serialize +inf") {
      val result = DoubleJsonFormat.serialize(Double.PositiveInfinity)
      result must failWith[JsonSerializationException]
    }

    it("must fail with JsonSerializationException when try to serialize -inf") {
      val result = DoubleJsonFormat.serialize(Double.NegativeInfinity)
      result must failWith[JsonSerializationException]
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
  import DoubleExtensions.DoubleWithIsDefined

  private lazy val triesPerTest = 100

  lazy val Doubles: Seq[Double] =
    RandomDataProvider
      .provideDoubles
      .filter(d => d.isDefined)
      .distinct
      .take(triesPerTest)
      .toList
}
