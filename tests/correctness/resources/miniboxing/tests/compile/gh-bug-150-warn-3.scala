package miniboxing.tests.compile.bug150.warn3

object Test {

  def test[@miniboxed X](t: X) = {
    class D {
      def foo(x: X): X = t
      protected def foo2(x: X): X = t
    }
    // TL;DR => we can't transform X to Long here: 
    // we could have a class E extends D here,
    // as shown in the miniboxing plugin bug #138
    // (github.com/miniboxing/miniboxing-plugin/issues/138)
    // in which case transforming D would be
    // incorrect (apparently, it may seem that
    // transforming both D and E consistently
    // will suffice, but this is not the case:
    // when encountering foo in E, it could
    // override a member from a transformed 
    // class, in the same specialized scope, or
    // a class that hasn't been transformed,
    // from the outer scope) => it's not safe
    // nor correct to transform foo here. Instead,
    // we should report a warning:
    new D
  }

  def main(args: Array[String]): Unit = {
    import language.reflectiveCalls
    println(test(3).foo(5))
  }
}
