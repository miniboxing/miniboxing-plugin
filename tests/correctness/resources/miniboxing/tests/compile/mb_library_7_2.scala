package miniboxing.tests.compile.library7.var2

// https://github.com/miniboxing/miniboxing-plugin/issues/44

trait GenSeqLike[@miniboxed +A] {
  override def equals(that: Any): Boolean = that match {
    case that: GenSeqLike[_] => true
    case _                   => false
  }
}
