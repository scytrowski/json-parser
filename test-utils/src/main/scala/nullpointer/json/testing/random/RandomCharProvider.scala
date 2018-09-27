package nullpointer.json.testing.random

import scala.util.Random

private object RandomCharProvider extends RandomObjectProvider[Char] {
  override def provide: Char = Random.nextPrintableChar
}
