package miniboxing.tests.compile
import miniboxing.plugin.minispec

class BUG3[@minispec T]() {
  def hashMode(): T = {
    val headhash: T = ???
    headhash
  }
}
