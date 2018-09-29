package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}
import nullpointer.json.JsonValues._
import nullpointer.json.testing.JsonFormatSpec
import org.scalamock.scalatest.MixedMockFactory

import scala.util.{Failure, Success}

class MapJsonFormatSpec extends JsonFormatSpec with MixedMockFactory {
  import MapJsonFormatSpec._

  describe("A MapJsonFormat") {
    it("must return JsonObject of serialized entries") {
      val firstEntryKey = mock[TestKey]
      val firstEntryValue = mock[TestValue]
      val secondEntryKey = mock[TestKey]
      val secondEntryValue = mock[TestValue]
      val serializedFirstEntryKey = JsonString("first key")
      val serializedFirstEntryValue = Proxy.mock[JsonValue]
      val serializedSecondEntryKey = JsonString("second key")
      val serializedSecondEntryValue = Proxy.mock[JsonValue]
      val keyFormat = mock[JsonFormat[TestKey]]
      (keyFormat.serialize _)
        .expects(firstEntryKey)
        .once()
        .returns(Success(serializedFirstEntryKey))
      (keyFormat.serialize _)
        .expects(secondEntryKey)
        .once()
        .returns(Success(serializedSecondEntryKey))
      val valueFormat = mock[JsonFormat[TestValue]]
      (valueFormat.serialize _)
        .expects(firstEntryValue)
        .once()
        .returns(Success(serializedFirstEntryValue))
      (valueFormat.serialize _)
        .expects(secondEntryValue)
        .once()
        .returns(Success(serializedSecondEntryValue))
      val mapFormat = new MapJsonFormat()(keyFormat, valueFormat)
      val result = mapFormat.serialize(Map(
        firstEntryKey -> firstEntryValue,
        secondEntryKey -> secondEntryValue
      ))
      result must succeedWith[JsonValue](JsonObject(
        serializedFirstEntryKey.value -> serializedFirstEntryValue,
        serializedSecondEntryKey.value -> serializedSecondEntryValue
      ))
    }

    it("must fail with JsonSerializationException if some serialized key is not JsonString") {
      val firstEntryKey = mock[TestKey]
      val firstEntryValue = mock[TestValue]
      val secondEntryKey = mock[TestKey]
      val secondEntryValue = mock[TestValue]
      val serializedFirstEntryKey = JsonString("some key")
      val serializedFirstEntryValue = Proxy.mock[JsonValue]
      val serializedSecondEntryKey = JsonNumber(1337)
      val serializedSecondEntryValue = Proxy.mock[JsonValue]
      val keyFormat = mock[JsonFormat[TestKey]]
      (keyFormat.serialize _)
        .expects(firstEntryKey)
        .once()
        .returns(Success(serializedFirstEntryKey))
      (keyFormat.serialize _)
        .expects(secondEntryKey)
        .once()
        .returns(Success(serializedSecondEntryKey))
      val valueFormat = mock[JsonFormat[TestValue]]
      (valueFormat.serialize _)
        .expects(firstEntryValue)
        .once()
        .returns(Success(serializedFirstEntryValue))
      val mapFormat = new MapJsonFormat()(keyFormat, valueFormat)
      val result = mapFormat.serialize(Map(
        firstEntryKey -> firstEntryValue,
        secondEntryKey -> secondEntryValue
      ))
      result must failWith[JsonSerializationException]
    }

    it("must fail with JsonSerializationException if serialization of some key fails") {
      val entryKey = mock[TestKey]
      val entryValue = mock[TestValue]
      val keyFormat = mock[JsonFormat[TestKey]]
      (keyFormat.serialize _)
        .expects(entryKey)
        .once()
        .returns(Failure(JsonSerializationException()))
      val valueFormat = mock[JsonFormat[TestValue]]
      val mapFormat = new MapJsonFormat()(keyFormat, valueFormat)
      val result = mapFormat.serialize(Map(entryKey -> entryValue))
      result must failWith[JsonSerializationException]
    }

    it("must fail with JsonSerializationException if serialization of some value fails") {
      val entryKey = mock[TestKey]
      val serializedEntryKey = JsonString("some key")
      val entryValue = mock[TestValue]
      val keyFormat = mock[JsonFormat[TestKey]]
      (keyFormat.serialize _)
        .expects(entryKey)
        .once()
        .returns(Success(serializedEntryKey))
      val valueFormat = mock[JsonFormat[TestValue]]
      (valueFormat.serialize _)
        .expects(entryValue)
        .once()
        .returns(Failure(JsonSerializationException()))
      val mapFormat = new MapJsonFormat()(keyFormat, valueFormat)
      val result = mapFormat.serialize(Map(entryKey -> entryValue))
      result must failWith[JsonSerializationException]
    }

    it("must deserialize correct map when JSON is JsonObject") {
      val firstEntryKeyJson = JsonString("some key")
      val firstEntryValueJson = Proxy.mock[JsonValue]
      val secondEntryKeyJson = JsonString("some another key")
      val secondEntryValueJson = Proxy.mock[JsonValue]
      val json = JsonObject(
        firstEntryKeyJson.value -> firstEntryValueJson,
        secondEntryKeyJson.value -> secondEntryValueJson)
      val deserializedFirstEntryKey = mock[TestKey]
      val deserializedFirstEntryValue = mock[TestValue]
      val deserializedSecondEntryKey = mock[TestKey]
      val deserializedSecondEntryValue = mock[TestValue]
      val keyFormat = mock[JsonFormat[TestKey]]
      (keyFormat.deserialize _)
        .expects(firstEntryKeyJson)
        .once()
        .returns(Success(deserializedFirstEntryKey))
      (keyFormat.deserialize _)
        .expects(secondEntryKeyJson)
        .once()
        .returns(Success(deserializedSecondEntryKey))
      val valueFormat = mock[JsonFormat[TestValue]]
      (valueFormat.deserialize _)
        .expects(firstEntryValueJson)
        .once()
        .returns(Success(deserializedFirstEntryValue))
      (valueFormat.deserialize _)
        .expects(secondEntryValueJson)
        .once()
        .returns(Success(deserializedSecondEntryValue))
      val mapFormat = new MapJsonFormat()(keyFormat, valueFormat)
      val result = mapFormat.deserialize(json)
      result must succeedWith(Map(
        deserializedFirstEntryKey -> deserializedFirstEntryValue,
        deserializedSecondEntryKey -> deserializedSecondEntryValue
      ))
    }

    it("must fail with JsonDeserializationException if JSON is not JsonObject") {
      val json = JsonNull
      val keyFormat = mock[JsonFormat[TestKey]]
      val valueFormat = mock[JsonFormat[TestValue]]
      val mapFormat = new MapJsonFormat()(keyFormat, valueFormat)
      val result = mapFormat.deserialize(json)
      result must failWith[JsonDeserializationException]
    }

    it("must fail with JsonDeserializationException if deserialization of some key fails") {
      val firstEntryKeyJson = JsonString("some key")
      val firstEntryValueJson = Proxy.mock[JsonValue]
      val secondEntryKeyJson = JsonString("some another key")
      val secondEntryValueJson = Proxy.mock[JsonValue]
      val deserializedFirstEntryKey = mock[TestKey]
      val deserializedFirstEntryValue = mock[TestValue]
      val deserializedSecondEntryValue = mock[TestValue]
      val keyFormat = stub[JsonFormat[TestKey]]
      (keyFormat.deserialize _)
        .when(firstEntryKeyJson)
        .returns(Success(deserializedFirstEntryKey))
      (keyFormat.deserialize _)
        .when(secondEntryKeyJson)
        .returns(Failure(JsonDeserializationException()))
      val valueFormat = stub[JsonFormat[TestValue]]
      (valueFormat.deserialize _)
        .when(firstEntryValueJson)
        .returns(Success(deserializedFirstEntryValue))
      (valueFormat.deserialize _)
        .when(secondEntryValueJson)
        .returns(Success(deserializedSecondEntryValue))
      val mapFormat = new MapJsonFormat()(keyFormat, valueFormat)
      val result = mapFormat.deserialize(JsonObject(
        firstEntryKeyJson.value -> firstEntryValueJson,
        secondEntryKeyJson.value -> secondEntryValueJson
      ))
      result must failWith[JsonDeserializationException]
    }

    it("must fail with JsonDeserializationException if deserialization of some value fails") {
      val firstEntryKeyJson = JsonString("some key")
      val firstEntryValueJson = Proxy.mock[JsonValue]
      val secondEntryKeyJson = JsonString("some another key")
      val secondEntryValueJson = Proxy.mock[JsonValue]
      val deserializedFirstEntryKey = mock[TestKey]
      val deserializedSecondEntryKey = mock[TestKey]
      val deserializedFirstEntryValue = mock[TestValue]
      val keyFormat = stub[JsonFormat[TestKey]]
      (keyFormat.deserialize _)
        .when(firstEntryKeyJson)
        .returns(Success(deserializedFirstEntryKey))
      (keyFormat.deserialize _)
        .when(secondEntryKeyJson)
        .returns(Success(deserializedSecondEntryKey))
      val valueFormat = stub[JsonFormat[TestValue]]
      (valueFormat.deserialize _)
        .when(firstEntryValueJson)
        .returns(Success(deserializedFirstEntryValue))
      (valueFormat.deserialize _)
        .when(secondEntryValueJson)
        .returns(Failure(JsonDeserializationException()))
      val mapFormat = new MapJsonFormat()(keyFormat, valueFormat)
      val result = mapFormat.deserialize(JsonObject(
        firstEntryKeyJson.value -> firstEntryValueJson,
        secondEntryKeyJson.value -> secondEntryValueJson
      ))
      result must failWith[JsonDeserializationException]
    }
  }
}

private object MapJsonFormatSpec {
  abstract class TestKey

  abstract class TestValue
}
