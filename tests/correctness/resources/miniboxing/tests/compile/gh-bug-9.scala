package miniboxing.tests.compile


class TailCallsCrash[@miniboxed T](val head: T) {
  final def containsTail(e: T): Boolean = {
    println(e)
    containsTail(e)
  }
}
