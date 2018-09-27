package nullpointer.json.testing.random

import scala.util.Random

private object RandomShortProvider extends RandomObjectProvider[Short] {
  override def provide: Short =
    (Random.nextInt(1 << 16) - (1 << 15)).toShort
}
