package miniboxing.tests.compile.bug95

object Test {
  def foo[@miniboxed T](t: T) = {
    //class C {  def bar(t: T): Int = 1 }
    //class D extends C {
    //  override def bar(t: T): Int = 2 }
    val t2: T = t
    //val d: D = new D
    //println(d.bar(t2)) // should print 2
    //val c: C = d
    //println(c.bar(t2)) // should print 2
    ()
  }
  
  def main(args: Array[String]) {
    foo(3)
  }
}
