package nullpointer.json.testing.extensions

object FloatExtensions {
  implicit class FloatWithIsDefined(float: Float) {
    def isDefined: Boolean =
      !float.isNaN && !float.isInfinite
  }
}
