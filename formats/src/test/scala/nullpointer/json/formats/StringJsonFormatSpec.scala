package nullpointer.json.formats

import nullpointer.json.JsonValues._
import nullpointer.json.formats.JsonFormatExceptions.JsonDeserializationException
import nullpointer.json.testing.{CommonSpec, TryMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.util.Random

class StringJsonFormatSpec extends CommonSpec with TryMatchers {
  describe("A StringJsonFormat") {
    it("must serialize to JsonString with correct value") {
      val serializeStringTestCases = Table(
        ("string", "expectedValue"),
        (1 to 100)
          .map(_ => Random.nextInt(100000))
          .map(n => Random.nextString(n))
          .map(s => s -> JsonString(s)):_*
      )
      forAll(serializeStringTestCases) { (string, expectedValue) =>
        val result = StringJsonFormat.serialize(string)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must deserialize JsonString to correct String") {
      val deserializeJsonStringTestCases = Table(
        ("json", "expectedString"),
        (1 to 100)
          .map(_ => Random.nextInt(100000))
          .map(n => Random.nextString(n))
          .map(s => JsonString(s) -> s):_*
      )
      forAll(deserializeJsonStringTestCases) { (json, expectedString) =>
        val result = StringJsonFormat.deserialize(json)
        result must succeedWith(expectedString)
      }
    }

    it("must fail with JsonDeserializationException on deserialization when JSON is not JsonString") {
      val failingDeserializationTestCases = Table(
        "json",
        JsonNull,
        JsonBoolean(false),
        JsonBoolean(true),
        JsonNumber(123),
        JsonArray(JsonBoolean(true), JsonNull),
        JsonObject("a" -> JsonNull, "b" -> JsonBoolean(false))
      )
      forAll(failingDeserializationTestCases) { json =>
        val result = StringJsonFormat.deserialize(json)
        result must failWith[JsonDeserializationException]
      }
    }
  }
}
