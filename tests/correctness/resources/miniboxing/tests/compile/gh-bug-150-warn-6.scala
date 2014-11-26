package miniboxing.tests.compile.bug150.warn6

object Test {

  def test[@miniboxed X](t: X) =
    new {
      // this time, the method becomes a part
      // of a structural type, therefore could 
      // potentially be called later (we don't
      // call it since it's not matching the
      // expected signature -- buggy reflective
      // calls, you know...) -- but we can call
      // it from the toString method...

      // Still, for some reason, the method
      // automatically becomes protected and
      // we can transform it... mmmkay...
      def foo(): X = t
      override def toString = "anon class for " + foo()
    }

  def main(args: Array[String]): Unit = {
    println(test(3).toString)
  }
}
