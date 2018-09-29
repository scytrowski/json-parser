package nullpointer.json.formats

import nullpointer.json.JsonValues._
import nullpointer.json.JsonFormatExceptions.JsonDeserializationException
import nullpointer.json.testing.JsonFormatSpec
import nullpointer.json.testing.random.RandomDataProvider
import org.scalatest.prop.TableDrivenPropertyChecks._

class LongJsonFormatSpec extends JsonFormatSpec {
  import LongJsonFormatSpec._

  describe("A LongJsonFormat") {
    it("must serialize to JsonNumber with correct value") {
      val serializeLongTestCases = Table(
        ("number", "expectedValue"),
        Longs.map(n => n -> JsonNumber(n)):_*
      )
      forAll(serializeLongTestCases) { (number, expectedValue) =>
        val result = LongJsonFormat.serialize(number)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must deserialize JsonNumber to correct Long") {
      val deserializeJsonNumberTestCases = Table(
        ("json", "expectedNumber"),
        Longs.map(n => JsonNumber(n) -> n):_*
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

private object LongJsonFormatSpec {
  private lazy val triesPerTest = 100

  lazy val Longs: Seq[Long] =
    RandomDataProvider
      .provideLongs
      .map(_ % (1 << 63)) // Prevent overflow issues - don't care about numbers greater than 2^63
      .distinct
      .take(triesPerTest)
      .toList
}
