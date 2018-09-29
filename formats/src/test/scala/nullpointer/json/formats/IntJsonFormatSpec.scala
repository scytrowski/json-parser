package nullpointer.json.formats

import nullpointer.json.JsonValues._
import nullpointer.json.JsonFormatExceptions.JsonDeserializationException
import nullpointer.json.testing.JsonFormatSpec
import nullpointer.json.testing.random.RandomDataProvider
import org.scalatest.prop.TableDrivenPropertyChecks._

class IntJsonFormatSpec extends JsonFormatSpec {
  import IntJsonFormatSpec._

  describe("An IntJsonFormat") {
    it("must serialize to JsonNumber with correct value") {
      val serializeIntTestCases = Table(
        ("number", "expectedValue"),
        Ints.map(n => n -> JsonNumber(n)):_*
      )
      forAll(serializeIntTestCases) { (number, expectedValue) =>
        val result = IntJsonFormat.serialize(number)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must deserialize JsonNumber to correct Int") {
      val deserializeJsonNumberTestCases = Table(
        ("json", "expectedNumber"),
        Ints.map(n => JsonNumber(n) -> n):_*
      )
      forAll(deserializeJsonNumberTestCases) { (json, expectedNumber) =>
        val result = IntJsonFormat.deserialize(json)
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
        val result = IntJsonFormat.deserialize(json)
        result must failWith[JsonDeserializationException]
      }
    }
  }
}

private object IntJsonFormatSpec {
  private lazy val triesPerTest = 100

  lazy val Ints: Seq[Int] =
    RandomDataProvider
      .provideInts
      .distinct
      .take(triesPerTest)
      .toList
}
