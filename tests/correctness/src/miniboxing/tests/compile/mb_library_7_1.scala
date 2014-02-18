package miniboxing.tests.compile.library7.var1

// https://github.com/miniboxing/miniboxing-plugin/issues/44

trait GenSeqLike[@miniboxed +A] {
  val a: A = ???
  def equals2(that: Any): A = that match {
    case that: GenSeqLike[_] => a
    case _                   => a
  }
}
