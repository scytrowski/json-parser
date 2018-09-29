package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonNull, JsonValue}
import nullpointer.json.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}
import nullpointer.json.testing.JsonFormatSpec
import org.scalamock.scalatest.MixedMockFactory

import scala.util.{Failure, Success}

class OptionJsonFormatSpec extends JsonFormatSpec with MixedMockFactory {
  import OptionJsonFormatSpec._

  describe("An OptionJsonFormat") {
    it("must return serialized element when option is defined") {
      val obj = mock[TestClass]
      val serializedElement = Proxy.mock[JsonValue]
      val elementFormat = mock[JsonFormat[TestClass]]
      (elementFormat.serialize _)
        .expects(obj)
        .once()
        .returns(Success(serializedElement))
      val optionFormat = new OptionJsonFormat()(elementFormat)
      val result = optionFormat.serialize(Some(obj))
      result must succeedWith[JsonValue](serializedElement)
    }

    it("must return JsonNull if option is not defined") {
      val elementFormat = mock[JsonFormat[TestClass]]
      val optionFormat = new OptionJsonFormat()(elementFormat)
      val result = optionFormat.serialize(None)
      result must succeedWith[JsonValue](JsonNull)
    }

    it("must fail with JsonSerializationException when element serialization fails") {
      val obj = mock[TestClass]
      val elementFormat = mock[JsonFormat[TestClass]]
      (elementFormat.serialize _)
        .expects(obj)
        .once()
        .returns(Failure(JsonSerializationException()))
      val optionFormat = new OptionJsonFormat()(elementFormat)
      val result = optionFormat.serialize(Some(obj))
      result must failWith[JsonSerializationException]
    }

    it("must return deserialized element when able to deserialize") {
      val json = Proxy.mock[JsonValue]
      val deserializedElement = mock[TestClass]
      val elementFormat = mock[JsonFormat[TestClass]]
      (elementFormat.deserialize _)
        .expects(json)
        .once()
        .returns(Success(deserializedElement))
      val optionFormat = new OptionJsonFormat()(elementFormat)
      val result = optionFormat.deserialize(json)
      result must succeedWith[Option[TestClass]](Some(deserializedElement))
    }

    it("must fail with JsonDeserializationException when unable to deserialize element") {
      val json = Proxy.mock[JsonValue]
      val elementFormat = mock[JsonFormat[TestClass]]
      (elementFormat.deserialize _)
        .expects(json)
        .once()
        .returns(Failure(JsonDeserializationException()))
      val optionFormat = new OptionJsonFormat()(elementFormat)
      val result = optionFormat.deserialize(json)
      result must failWith[JsonDeserializationException]
    }

    it("must deserialize None when JSON is JsonNull") {
      val elementFormat = mock[JsonFormat[TestClass]]
      val optionFormat = new OptionJsonFormat()(elementFormat)
      val result = optionFormat.deserialize(JsonNull)
      result must succeedWith[Option[TestClass]](None)
    }
  }
}

private object OptionJsonFormatSpec {
  abstract class TestClass
}
