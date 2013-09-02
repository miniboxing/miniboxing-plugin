package miniboxing.tests.compile


// Note the override problem shown here also affects specialization, 
// see SI-7659: https://issues.scala-lang.org/browse/SI-7659

trait Base21[@miniboxed T, S] {
  def overrideMe(t: T, s: S): T = t
}

trait Base22[@miniboxed T, S] {
  def overrideMe(t: T, s: S): T = t
}

trait Base23[@miniboxed T, @miniboxed S] {
  def overrideMe(t: T, s: S): T = t
}

class LongIsBase21Base22andBase23 extends Base21[Long, Long] with Base22[Long, Long] with Base23[Long, Long] {
  override def overrideMe(t: Long, s: Long): Long = ???
}

