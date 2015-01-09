package miniboxing.tests.compile.bug169

trait CommutativeSemigroup[@miniboxed A] extends Any { self =>
  def foo = ???
}

trait Semilattice[@miniboxed A] extends Any with CommutativeSemigroup[A] { self =>
  def bar = ???
}

trait BoundedSemilattice[@miniboxed A] extends Any with Semilattice[A] { self =>

  override def foo = ???
  override def bar = ???
}

