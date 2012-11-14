class BUG7[@miniboxing.plugin.minispec T](head: T, tail: BUG7[T]) {
  //override def hashCode(): Int = 123
  override def hashCode(): Int = tail.hashCode
}
