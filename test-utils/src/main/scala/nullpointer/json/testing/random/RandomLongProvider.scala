package nullpointer.json.testing.random

import scala.util.Random

private object RandomLongProvider extends RandomObjectProvider[Long] {
  override def provide: Long = Random.nextLong
}
