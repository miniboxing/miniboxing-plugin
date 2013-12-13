package miniboxing.tests.correctness.erasure.torture5

class A[@miniboxed T <: Int]() {
  def foo(c: T): Any = {
    @annotation.tailrec
    def foo_tail(c: T, cond: Boolean): T = 
      if (cond)
        foo_tail(c, !cond)
      else
        c
    foo_tail(c, true)
  }
}
