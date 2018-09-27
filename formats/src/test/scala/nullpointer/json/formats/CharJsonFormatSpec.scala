package nullpointer.json.formats

import nullpointer.json.JsonValues._
import nullpointer.json.formats.JsonFormatExceptions.JsonDeserializationException
import nullpointer.json.testing.JsonFormatSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.util.Random

class CharJsonFormatSpec extends JsonFormatSpec {
  describe("A CharJsonFormat") {
    it("must serialize to JsonString with correct value") {
      val serializeCharTestCases = Table(
        ("char", "expectedValue"),
        (1 to 100)
          .map(_ => Random.nextPrintableChar)
          .map(c => c -> JsonString(c.toString)):_*
      )
      forAll(serializeCharTestCases) { (char, expectedValue) =>
        val result = CharJsonFormat.serialize(char)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must deserialize JsonString with single-char value to correct Char") {
      val deserializeSingleCharJsonStringTestCases = Table(
        ("json", "expectedChar"),
        (1 to 100)
          .map(_ => Random.nextPrintableChar)
          .map(c => JsonString(c.toString) -> c):_*
      )
      forAll(deserializeSingleCharJsonStringTestCases) { (json, expectedChar) =>
        val result = CharJsonFormat.deserialize(json)
        result must succeedWith(expectedChar)
      }
    }

    it("must fail with JsonDeserializationException on deserialization when JSON is JsonString with empty value") {
      val result = CharJsonFormat.deserialize(JsonString(""))
      result must failWith[JsonDeserializationException]
    }

    it("must fail with JsonDeserializationException on deserialization when JSON is JsonString with multi-char value") {
      val failingDeserializationWithMultiCharValueTestCases = Table(
        "value",
        "ab",
        "abc",
        "abcd",
        "!@",
        "bmlopwl$@$mn%!"
      )
      forAll(failingDeserializationWithMultiCharValueTestCases) { value =>
        val result = CharJsonFormat.deserialize(JsonString(value))
        result must failWith[JsonDeserializationException]
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
        val result = CharJsonFormat.deserialize(json)
        result must failWith[JsonDeserializationException]
      }
    }
  }
}
