package nullpointer.json

import nullpointer.json.formats._

import scala.util.Try

trait BasicJsonFormats {
  implicit lazy val BooleanFormat: JsonFormat[Boolean] = BooleanJsonFormat

  implicit lazy val ByteFormat: JsonFormat[Byte] = ByteJsonFormat

  implicit lazy val ShortFormat: JsonFormat[Short] = ShortJsonFormat

  implicit lazy val IntFormat: JsonFormat[Int] = IntJsonFormat

  implicit lazy val LongFormat: JsonFormat[Long] = LongJsonFormat

  implicit lazy val FloatFormat: JsonFormat[Float] = FloatJsonFormat

  implicit lazy val DoubleFormat: JsonFormat[Double] = DoubleJsonFormat

  implicit lazy val CharFormat: JsonFormat[Char] = CharJsonFormat

  implicit lazy val StringFormat: JsonFormat[String] = StringJsonFormat

  implicit def OptionFormat[E: JsonFormat]: JsonFormat[Option[E]] = new OptionJsonFormat[E]

  implicit def TryFormat[R: JsonFormat]: JsonFormat[Try[R]] = new TryJsonFormat[R]

  implicit def SeqFormat[E: JsonFormat]: JsonFormat[Seq[E]] = new SeqJsonFormat[E]

  implicit def MapFormat[K: JsonFormat, V: JsonFormat]: JsonFormat[Map[K, V]] = new MapJsonFormat[K, V]
}
