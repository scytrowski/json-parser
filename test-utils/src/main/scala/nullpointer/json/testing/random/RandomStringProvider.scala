package nullpointer.json.testing.random

import scala.util.Random

private class RandomStringProvider private(minimumLength: Int, maximumLength: Int) extends RandomObjectProvider[String] {
  override def provide: String = {
    val length = Random.nextInt(maximumLength - minimumLength) + minimumLength
    Random.nextString(length)
  }
}

private object RandomStringProvider {
  def apply(minimumLength: Int, maximumLength: Int): RandomStringProvider =
    new RandomStringProvider(minimumLength, maximumLength)
}
