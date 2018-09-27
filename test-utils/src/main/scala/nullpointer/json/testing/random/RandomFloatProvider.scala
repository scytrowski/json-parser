package nullpointer.json.testing.random

import scala.util.Random

private object RandomFloatProvider extends RandomObjectProvider[Float] {
  override def provide: Float =
    Random.nextFloat * (Float.MaxValue - Float.MinValue) + Float.MinValue
}
