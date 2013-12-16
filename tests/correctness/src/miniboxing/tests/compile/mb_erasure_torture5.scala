package miniboxing.tests.correctness.erasure.torture5

/** Tailrec conversion test for mb/erasure */
class A[@miniboxed T <: Int]() {
  def foo(c: T): Any = {
    // the recursive call should still be in
    // tailcall position after the conversion,
    // meaning no redundant conversions have
    // been added to the tree:
    @annotation.tailrec
    def foo_tail(c: T, cond: Boolean): T = 
      if (cond)
        foo_tail(c, !cond)
      else
        c
    foo_tail(c, true)
  }
}
