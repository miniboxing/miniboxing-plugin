package miniboxing.tests.compile
import miniboxing.plugin.minispec

class BUG8[@minispec T](tail: BUG8[T]) {
  def contains(e: T): Boolean = tail.contains(e)
}
