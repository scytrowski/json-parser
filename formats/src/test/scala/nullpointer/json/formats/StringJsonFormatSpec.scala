package nullpointer.json.formats

import nullpointer.json.JsonValues._
import nullpointer.json.JsonFormatExceptions.JsonDeserializationException
import nullpointer.json.testing.JsonFormatSpec
import nullpointer.json.testing.random.RandomDataProvider
import org.scalatest.prop.TableDrivenPropertyChecks._

class StringJsonFormatSpec extends JsonFormatSpec {
  import StringJsonFormatSpec._

  describe("A StringJsonFormat") {
    it("must serialize to JsonString with correct value") {
      val serializeStringTestCases = Table(
        ("string", "expectedValue"),
        Strings.map(s => s -> JsonString(s)):_*
      )
      forAll(serializeStringTestCases) { (string, expectedValue) =>
        val result = StringJsonFormat.serialize(string)
        result must succeedWith[JsonValue](expectedValue)
      }
    }

    it("must deserialize JsonString to correct String") {
      val deserializeJsonStringTestCases = Table(
        ("json", "expectedString"),
        Strings.map(s => JsonString(s) -> s):_*
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

private object StringJsonFormatSpec {
  private lazy val triesPerTest = 100
  private lazy val minimumLength = 1
  private lazy val maximumLength = 10000

  lazy val Strings: Seq[String] =
    RandomDataProvider
      .provideStrings(minimumLength, maximumLength)
      .distinct
      .take(triesPerTest)
      .toList
}
