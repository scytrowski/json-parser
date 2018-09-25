package nullpointer.json.formats

import nullpointer.json.JsonValues._
import nullpointer.json.formats.JsonFormatExceptions.JsonDeserializationException
import nullpointer.json.testing.{CommonSpec, TryMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.util.Random

class FloatJsonFormatSpec extends CommonSpec with TryMatchers {
  describe("A FloatJsonFormat") {
    it("must serialize to JsonNumber with correct value") {
      val serializeFloatTestCases = Table(
        ("number", "expectedValue"),
        (1 to 100)
          .map(_ => Random.nextFloat)
          .map(n => n -> JsonNumber(n)):_*
      )
      forAll(serializeFloatTestCases) { (number, expectedValue) =>
        val result = FloatJsonFormat.serialize(number)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must deserialize JsonNumber to correct Float") {
      val deserializeJsonNumberTestCases = Table(
        ("json", "expectedNumber"),
        (1 to 100)
          .map(_ => Random.nextDouble)
          .map(n => JsonNumber(n) -> n.toFloat):_*
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
