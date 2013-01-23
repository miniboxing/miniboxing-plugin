package miniboxing.bugs
import miniboxing.plugin.minispec

class BUG3[@minispec T]() {
  def hashMode(): T = {
    val headhash: T = ???
    headhash
  }
}
