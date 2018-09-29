package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.JsonValue
import nullpointer.json.formats.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}
import nullpointer.json.testing.JsonFormatSpec
import org.scalamock.scalatest.MixedMockFactory

import scala.util.{Failure, Success, Try}

class TryJsonFormatSpec extends JsonFormatSpec with MixedMockFactory {
  import TryJsonFormatSpec._

  describe("A TryJsonFormat") {
    it("must return serialized result when try succeeded") {
      val obj = mock[TestClass]
      val serializedResult = Proxy.mock[JsonValue]
      val elementFormat = mock[JsonFormat[TestClass]]
      (elementFormat.serialize _)
        .expects(obj)
        .once()
        .returns(Success(serializedResult))
      val tryFormat = new TryJsonFormat()(elementFormat)
      val result = tryFormat.serialize(Success(obj))
      result must succeedWith[JsonValue](serializedResult)
    }

    it("must fail with JsonSerializationException when try fails") {
      val elementFormat = mock[JsonFormat[TestClass]]
      val tryFormat = new TryJsonFormat()(elementFormat)
      val result = tryFormat.serialize(Failure(JsonSerializationException()))
      result must failWith[JsonSerializationException]
    }

    it("must fail with JsonSerializationException when unable to serialize result") {
      val obj = mock[TestClass]
      val elementFormat = mock[JsonFormat[TestClass]]
      (elementFormat.serialize _)
        .expects(obj)
        .once()
        .returns(Failure(JsonSerializationException()))
      val tryFormat = new TryJsonFormat()(elementFormat)
      val result = tryFormat.serialize(Success(obj))
      result must failWith[JsonSerializationException]
    }

    it("must return deserialized element when able to deserialize result") {
      val json = Proxy.mock[JsonValue]
      val deserializedResult = mock[TestClass]
      val elementFormat = mock[JsonFormat[TestClass]]
      (elementFormat.deserialize _)
        .expects(json)
        .once()
        .returns(Success(deserializedResult))
      val tryFormat = new TryJsonFormat()(elementFormat)
      val result = tryFormat.deserialize(json)
      result must succeedWith[Try[TestClass]](Success(deserializedResult))
    }

    it("must fail with JsonDeserializationException when unable to deserialize result") {
      val json = Proxy.mock[JsonValue]
      val elementFormat = mock[JsonFormat[TestClass]]
      (elementFormat.deserialize _)
        .expects(json)
        .once()
        .returns(Failure(JsonDeserializationException()))
      val tryFormat = new TryJsonFormat()(elementFormat)
      val result = tryFormat.deserialize(json)
      result must failWith[JsonDeserializationException]
    }
  }
}

private object TryJsonFormatSpec {
  abstract class TestClass
}
