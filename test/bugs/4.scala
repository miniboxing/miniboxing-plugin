package miniboxing.bugs
import miniboxing.plugin.minispec

class BUG[@minispec T]() {
  def hashMode(t: T): Int = {
    val headhash: Int = 12
    headhash
  }
}
