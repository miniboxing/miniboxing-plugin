package miniboxing.tests.compile.bug85.reduced

trait MakeImplClass[@miniboxed A] {
  def foo(a: A): A
}
