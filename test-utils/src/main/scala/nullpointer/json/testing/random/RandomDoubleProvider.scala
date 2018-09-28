package nullpointer.json.testing.random

import java.nio.{ByteBuffer, ByteOrder}

import scala.util.Random

private object RandomDoubleProvider extends RandomObjectProvider[Double] {
  override def provide: Double =
    long2double(Random.nextLong)

  private def long2double(long: Long): Double = {
    val longBytes: Array[Byte] = Array(
      (long & 0xFF).toByte,
      ((long >> 8) & 0xFF).toByte,
      ((long >> 16) & 0xFF).toByte,
      ((long >> 24) & 0xFF).toByte,
      ((long >> 32) & 0xFF).toByte,
      ((long >> 40) & 0xFF).toByte,
      ((long >> 48) & 0xFF).toByte,
      ((long >> 56) & 0xFF).toByte
    )
    val longBytesBuffer = ByteBuffer.wrap(longBytes)
      .order(ByteOrder.LITTLE_ENDIAN)
    longBytesBuffer.getDouble(0)
  }
}
