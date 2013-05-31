import miniboxing.plugin.minispec

class LocalValuesHandling[@minispec T] {
  private[this] val x: Int = 0
  def extend(): Int = x + 1
}

