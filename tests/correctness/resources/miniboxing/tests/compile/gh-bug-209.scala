package mbhashmap

class MbHashmap {       
  def builder[@miniboxed U, @miniboxed V](x: (U, V)) = {
    val u = x._2
    val v = x._1
    val tup = (u, v)
    tup
  }
}
