package miniboxing.tests.compile


class BUG4[@miniboxed T]() {
  def hashMode(t: T): Int = {
    val headhash: Int = 12
    headhash
  }
}
