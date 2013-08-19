package miniboxing.tests.compile



class VerifyArrayOps[@miniboxed T: Manifest] {
  val x = new Array[T](10)
  def reverse() = {
    var i = 0
    var j = x.length - 1
    while (i < j) {
      val t1: T = x(i)
      val t2: T = x(j)
      x(i) = t2
      x(j) = t1
      i += 1
      j -= 1
    }
  }
}
