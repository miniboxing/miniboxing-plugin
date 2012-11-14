class BUG8[@miniboxing.plugin.minispec T](tail: BUG8[T]) {
  def contains(e: T): Boolean = tail.contains(e)
}
