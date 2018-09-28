package nullpointer.json.testing.random

import java.nio.{ByteBuffer, ByteOrder}

import scala.util.Random

private object RandomFloatProvider extends RandomObjectProvider[Float] {
  override def provide: Float =
    int2float(Random.nextInt)

  private def int2float(int: Int): Float = {
    val intBytes: Array[Byte] = Array(
      (int & 0xFF).toByte,
      ((int >> 8) & 0xFF).toByte,
      ((int >> 16) & 0xFF).toByte,
      ((int >> 24) & 0xFF).toByte
    )
    val intBytesBuffer = ByteBuffer.wrap(intBytes)
      .order(ByteOrder.LITTLE_ENDIAN)
    intBytesBuffer.getFloat(0)
  }
}
