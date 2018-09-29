package nullpointer.json.testing.extensions

object DoubleExtensions {
  implicit class DoubleWithIsDefined(double: Double) {
    def isDefined: Boolean =
      !double.isNaN && !double.isInfinite
  }
}
