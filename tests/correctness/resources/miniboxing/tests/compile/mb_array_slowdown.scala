package miniboxing.example

import MbArray._

object Test {
  final val len = 20000000
 
  def sumB(ary: MbArray[Int]) = {
    var i = 0
    while (i < len) {
      ary(i) = 1
      ary(0) = ary(0) + ary(i)
      i += 1
    }
    ary(0)
  }
}
