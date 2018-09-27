package nullpointer.json.testing.random

object RandomDataProvider {
  def provideBytes: Stream[Byte] = provideStream(RandomByteProvider)

  def provideShorts: Stream[Short] = provideStream(RandomShortProvider)

  def provideInts: Stream[Int] = provideStream(RandomIntProvider)

  def provideLongs: Stream[Long] = provideStream(RandomLongProvider)

  def provideFloats: Stream[Float] = provideStream(RandomFloatProvider)

  def provideDoubles: Stream[Double] = provideStream(RandomDoubleProvider)

  def provideChars: Stream[Char] = provideStream(RandomCharProvider)

  def provideStrings(minimumLength: Int, maximumLength: Int): Stream[String] =
    provideStream(RandomStringProvider(minimumLength, maximumLength))

  def provideStream[T](objectProvider: RandomObjectProvider[T]): Stream[T] =
    Stream.continually(objectProvider())
}
