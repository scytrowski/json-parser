package nullpointer.json.testing.random

import scala.util.Random

private object RandomIntProvider extends RandomObjectProvider[Int] {
  override def provide: Int = Random.nextInt
}
