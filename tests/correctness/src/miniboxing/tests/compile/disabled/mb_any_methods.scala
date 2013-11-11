package miniboxing.tests.compile



class CCC[@miniboxed T](val t: T) {
  def foo(t1: T, t2: Any) = {
    t.hashCode
    t == t1
    t == t2
    t.toString
  }
}
