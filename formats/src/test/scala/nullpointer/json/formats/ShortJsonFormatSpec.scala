package nullpointer.json.formats

import nullpointer.json.JsonValues._
import nullpointer.json.formats.JsonFormatExceptions.JsonDeserializationException
import nullpointer.json.testing.{CommonSpec, JsonFormatSpec, TryMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.util.Random

class ShortJsonFormatSpec extends JsonFormatSpec {
  import ShortJsonFormatSpec._

  describe("A ShortJsonFormat") {
    it("must serialize to JsonNumber with correct value") {
      val serializeShortTestCases = Table(
        ("number", "expectedValue"),
        (1 to 100)
          .map(_ => nextShort)
          .map(n => n -> JsonNumber(n)):_*
      )
      forAll(serializeShortTestCases) { (number, expectedValue) =>
        val result = ShortJsonFormat.serialize(number)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must deserialize JsonNumber to correct Short") {
      val deserializeJsonNumberTestCases = Table(
        ("json", "expectedNumber"),
        (1 to 100)
          .map(_ => Random.nextDouble)
          .map(n => JsonNumber(n) -> n.toShort):_*
      )
      forAll(deserializeJsonNumberTestCases) { (json, expectedNumber) =>
        val result = ShortJsonFormat.deserialize(json)
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
        val result = ShortJsonFormat.deserialize(json)
        result must failWith[JsonDeserializationException]
      }
    }
  }
}

private object ShortJsonFormatSpec {
  private lazy val nextShortUpperBound: Int = Short.MaxValue + Short.MinValue.toInt.abs

  def nextShort: Short =
    (Random.nextInt(nextShortUpperBound) - Short.MinValue.abs).toShort
}
