package miniboxing.bugs
import miniboxing.plugin.minispec

class BUG7[@minispec T](head: T, tail: BUG7[T]) {
  //override def hashCode(): Int = 123
  override def hashCode(): Int = tail.hashCode
}
