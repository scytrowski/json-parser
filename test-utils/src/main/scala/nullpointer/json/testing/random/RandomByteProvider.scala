package nullpointer.json.testing.random

import scala.util.Random

private object RandomByteProvider extends RandomObjectProvider[Byte] {
  override def provide: Byte =
    (Random.nextInt(1 << 8) - (1 << 7)).toByte
}
