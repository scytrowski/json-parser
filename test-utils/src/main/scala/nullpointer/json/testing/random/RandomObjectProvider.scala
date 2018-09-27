package nullpointer.json.testing.random

trait RandomObjectProvider[T] {
  def apply(): T = provide

  def provide: T
}
