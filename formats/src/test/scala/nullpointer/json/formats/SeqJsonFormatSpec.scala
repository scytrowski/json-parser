package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonValues.{JsonArray, JsonNull, JsonValue}
import nullpointer.json.formats.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}
import nullpointer.json.testing.JsonFormatSpec
import org.scalamock.scalatest.MixedMockFactory

import scala.util.{Failure, Success}

class SeqJsonFormatSpec extends JsonFormatSpec with MixedMockFactory {
  import SeqJsonFormatSpec._

  describe("A SeqJsonFormat") {
    it("must return JsonArray of serialized elements") {
      val firstElement = mock[TestClass]
      val serializedFirstElement = Proxy.mock[JsonValue]
      val secondElement = mock[TestClass]
      val serializedSecondElement = Proxy.mock[JsonValue]
      val elementFormat = mock[JsonFormat[TestClass]]
      (elementFormat.serialize _)
        .expects(firstElement)
        .once()
        .returns(Success(serializedFirstElement))
      (elementFormat.serialize _)
        .expects(secondElement)
        .once()
        .returns(Success(serializedSecondElement))
      val seqFormat = new SeqJsonFormat()(elementFormat)
      val result = seqFormat.serialize(Seq(firstElement, secondElement))
      result must succeedWith[JsonValue](JsonArray(serializedFirstElement, serializedSecondElement))
    }

    it("must fail with JsonSerializationException when serialization of any element fails") {
      val firstElement = mock[TestClass]
      val serializedFirstElement = Proxy.mock[JsonValue]
      val secondElement = mock[TestClass]
      val elementFormat = mock[JsonFormat[TestClass]]
      (elementFormat.serialize _)
        .expects(firstElement)
        .once()
        .returns(Success(serializedFirstElement))
      (elementFormat.serialize _)
        .expects(secondElement)
        .once()
        .returns(Failure(JsonSerializationException()))
      val seqFormat = new SeqJsonFormat()(elementFormat)
      val result = seqFormat.serialize(Seq(firstElement, secondElement))
      result must failWith[JsonSerializationException]
    }

    it("must return Seq of deserialized elements when JSON is JsonArray") {
      val firstElementJson = Proxy.mock[JsonValue]
      val firstElement = mock[TestClass]
      val secondElementJson = Proxy.mock[JsonValue]
      val secondElement = mock[TestClass]
      val elementFormat = mock[JsonFormat[TestClass]]
      (elementFormat.deserialize _)
        .expects(firstElementJson)
        .once()
        .returns(Success(firstElement))
      (elementFormat.deserialize _)
        .expects(secondElementJson)
        .once()
        .returns(Success(secondElement))
      val seqFormat = new SeqJsonFormat()(elementFormat)
      val result = seqFormat.deserialize(JsonArray(firstElementJson, secondElementJson))
      result must succeedWith(Seq(firstElement, secondElement))
    }

    it("must fail with JsonDeserializationException when JSON is not JsonArray") {
      val elementFormat = mock[JsonFormat[TestClass]]
      val seqFormat = new SeqJsonFormat()(elementFormat)
      val result = seqFormat.deserialize(JsonNull)
      result must failWith[JsonDeserializationException]
    }

    it("must fail with JsonDeserializationException when deserialization of any element fails") {
      val firstElementJson = Proxy.mock[JsonValue]
      val firstElement = mock[TestClass]
      val secondElementJson = Proxy.mock[JsonValue]
      val elementFormat = mock[JsonFormat[TestClass]]
      (elementFormat.deserialize _)
        .expects(firstElementJson)
        .once()
        .returns(Success(firstElement))
      (elementFormat.deserialize _)
        .expects(secondElementJson)
        .once()
        .returns(Failure(JsonDeserializationException()))
      val seqFormat = new SeqJsonFormat()(elementFormat)
      val result = seqFormat.deserialize(JsonArray(firstElementJson, secondElementJson))
      result must failWith[JsonDeserializationException]
    }
  }
}

private object SeqJsonFormatSpec {
  abstract class TestClass
}