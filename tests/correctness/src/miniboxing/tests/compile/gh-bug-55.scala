package miniboxing.tests.compile.bug55

object `$iw` {
  object Test1 {
    def foo[@miniboxed T](t: T): T = 
      throw new Exception("Innermost performance-critical method")
    def bar[@miniboxed T](t: T): T = foo(t)
    def baz[@miniboxed T](t: T): T = bar(t)
  }  

  def trace(statement: => Unit): Unit =
    try {
      statement
    } catch {
      case e: Exception => 
        // prettify the trace and output it:
        for (frame <- e.getStackTrace.take(3))
          println(frame.toString.replaceAll(".*Test", "Test"))
    }

  object Test3 {
    class Z[@miniboxed T](t: T) {
      def foo() = trace(Test1.baz(t))
    }

    val z_opt: Z[_] = new Z(3)
    val z_gen: Z[_] = new Z("x")
  }
}
