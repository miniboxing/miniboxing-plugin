package miniboxing.tests.compile


class BUG3[@miniboxed T]() {
  def hashMode(): T = {
    val headhash: T = ???
    headhash
  }
}
