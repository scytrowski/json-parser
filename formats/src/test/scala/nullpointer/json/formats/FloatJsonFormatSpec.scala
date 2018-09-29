package nullpointer.json.formats

import nullpointer.json.JsonValues._
import nullpointer.json.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}
import nullpointer.json.testing.JsonFormatSpec
import nullpointer.json.testing.random.RandomDataProvider
import nullpointer.json.testing.extensions.FloatExtensions
import org.scalatest.prop.TableDrivenPropertyChecks._

class FloatJsonFormatSpec extends JsonFormatSpec {
  import FloatJsonFormatSpec._

  describe("A FloatJsonFormat") {
    it("must serialize to JsonNumber with correct value") {
      val serializeFloatTestCases = Table(
        ("number", "expectedValue"),
        Floats.map(n => n -> JsonNumber(n)):_*
      )
      forAll(serializeFloatTestCases) { (number, expectedValue) =>
        val result = FloatJsonFormat.serialize(number)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must fail with JsonSerializationException when try to serialize NaN") {
      val result = FloatJsonFormat.serialize(Float.NaN)
      result must failWith[JsonSerializationException]
    }

    it("must fail with JsonSerializationException when try to serialize +inf") {
      val result = FloatJsonFormat.serialize(Float.PositiveInfinity)
      result must failWith[JsonSerializationException]
    }

    it("must fail with JsonSerializationException when try to serialize -inf") {
      val result = FloatJsonFormat.serialize(Float.NegativeInfinity)
      result must failWith[JsonSerializationException]
    }

    it("must deserialize JsonNumber to correct Float") {
      val deserializeJsonNumberTestCases = Table(
        ("json", "expectedNumber"),
        Floats.map(n => JsonNumber(n) -> n):_*
      )
      forAll(deserializeJsonNumberTestCases) { (json, expectedNumber) =>
        val result = FloatJsonFormat.deserialize(json)
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
        val result = FloatJsonFormat.deserialize(json)
        result must failWith[JsonDeserializationException]
      }
    }
  }
}

private object FloatJsonFormatSpec {
  import FloatExtensions.FloatWithIsDefined

  private lazy val triesPerTest = 100

  lazy val Floats: Seq[Float] =
    RandomDataProvider
      .provideFloats
      .filter(f => f.isDefined)
      .distinct
      .take(triesPerTest)
      .toList
}
