package nullpointer.json.testing.random

import scala.util.Random

private object RandomDoubleProvider extends RandomObjectProvider[Double] {
  override def provide: Double =
    Random.nextDouble * (Double.MaxValue - Double.MinValue) + Double.MinValue
}
