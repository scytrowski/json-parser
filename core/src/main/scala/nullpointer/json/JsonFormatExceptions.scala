package nullpointer.json

object JsonFormatExceptions {
  abstract class JsonFormatException(message: String = null, cause: Throwable = null) extends Exception

  final case class JsonSerializationException(message: String = null, cause: Throwable = null) extends JsonFormatException

  final case class JsonDeserializationException(message: String = null, cause: Throwable = null) extends JsonFormatException
}
