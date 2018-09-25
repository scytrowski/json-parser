package nullpointer.json.formats

import nullpointer.json.JsonValues._
import nullpointer.json.formats.JsonFormatExceptions.JsonDeserializationException
import nullpointer.json.testing.{CommonSpec, TryMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.util.Random

class LongJsonFormatSpec extends CommonSpec with TryMatchers {
  describe("A LongJsonFormat") {
    it("must serialize to JsonNumber with correct value") {
      // TODO: Check overflow issues during Long -> Double conversion
      val serializeLongTestCases = Table(
        ("number", "expectedValue"),
        (1 to 100)
          .map(_ => Random.nextLong)
          .map(n => n -> JsonNumber(n)):_*
      )
      forAll(serializeLongTestCases) { (number, expectedValue) =>
        val result = LongJsonFormat.serialize(number)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must deserialize JsonNumber to correct Long") {
      val deserializeJsonNumberTestCases = Table(
        ("json", "expectedNumber"),
        (1 to 100)
          .map(_ => Random.nextDouble)
          .map(n => JsonNumber(n) -> n.toLong):_*
      )
      forAll(deserializeJsonNumberTestCases) { (json, expectedNumber) =>
        val result = LongJsonFormat.deserialize(json)
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
        val result = LongJsonFormat.deserialize(json)
        result must failWith[JsonDeserializationException]
      }
    }
  }
}
