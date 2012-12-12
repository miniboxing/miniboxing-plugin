package miniboxing.bugs
import miniboxing.plugin.minispec

class BUG4[@minispec T]() {
  def hashMode(t: T): Int = {
    val headhash: Int = 12
    headhash
  }
}
