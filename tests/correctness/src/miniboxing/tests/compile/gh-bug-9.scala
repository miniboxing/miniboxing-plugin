package miniboxing.tests.compile
import miniboxing.plugin.minispec

class TailCallsCrash[@minispec T](val head: T) {
  final def containsTail(e: T): Boolean = {
    println(e)
    containsTail(e)
  }
}
