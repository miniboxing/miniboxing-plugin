package miniboxing.tests.compile



class LocalValuesHandling[@miniboxed T] {
  private[this] val x: Int = 0
  def extend(): Int = x + 1
}

