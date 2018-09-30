package nullpointer.json.formats

import nullpointer.json.JsonFormat
import nullpointer.json.JsonFormatExceptions.{JsonDeserializationException, JsonSerializationException}
import nullpointer.json.JsonValues.{JsonObject, JsonString, JsonValue}

import scala.util.{Failure, Success, Try}

class MapJsonFormat[K, V](implicit keyFormat: JsonFormat[K], valueFormat: JsonFormat[V]) extends JsonFormat[Map[K, V]] {
  import SeqOfTriesExtensions.SeqOfTriesWithAllSucceeded

  override def serialize(obj: Map[K, V]): Try[JsonValue] =
    obj.map {
      case (key, value) =>
        for {
          keyJson <- serializeKey(key)
          valueJson <- serializeValue(value)
        } yield keyJson.value -> valueJson
    }.toSeq
      .allSucceeded
      .map(e => JsonObject(e.toMap))

  override def deserialize(json: JsonValue): Try[Map[K, V]] = json match {
    case jsonObject: JsonObject => deserializeJsonObject(jsonObject)
    case _ => Failure(JsonDeserializationException(s"Cannot deserialize JSON object key from $json"))
  }

  private def serializeKey(key: K): Try[JsonString] =
    keyFormat.serialize(key) match {
      case Success(keyJson) => geyKeyJsonString(keyJson)
      case Failure(exception) =>
        Failure(JsonSerializationException("Cannot serialize map due to some entry key serialization exception", exception))
    }

  private def geyKeyJsonString(keyJson: JsonValue): Try[JsonString] =
    keyJson match {
      case keyString: JsonString => Success(keyString)
      case _ =>
        Failure(JsonSerializationException("Map key must be serializable to JsonString"))
    }

  private def serializeValue(value: V): Try[JsonValue] =
    valueFormat.serialize(value) match {
      case success: Success[JsonValue] => success
      case Failure(exception) =>
        Failure(JsonSerializationException("Cannot serialize map due to some entry value serialization exception", exception))
    }

  private def deserializeJsonObject(jsonObject: JsonObject): Try[Map[K, V]] =
    jsonObject.elements.map {
      case (key, valueJson) =>
        for {
          deserializedKey <- deserializeJsonObjectKey(key)
          deserializedValue <- deserializeJsonObjectValue(valueJson)
        } yield deserializedKey -> deserializedValue
    }.toSeq
      .allSucceeded
      .map(_.toMap)

  private def deserializeJsonObjectKey(key: String): Try[K] =
    keyFormat.deserialize(JsonString(key)) match {
      case success @ Success(_) => success
      case Failure(exception) =>
        Failure(JsonDeserializationException("Cannot deserialize map due to some entry key deserialization exception", exception))
    }

  private def deserializeJsonObjectValue(valueJson: JsonValue): Try[V] =
    valueFormat.deserialize(valueJson) match {
      case success @ Success(_) => success
      case Failure(exception) =>
        Failure(JsonDeserializationException("Cannot deserialize map due to some entry value deserialization exception", exception))
    }
}
