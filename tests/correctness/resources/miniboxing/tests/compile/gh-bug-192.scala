package miniboxing.tests.compile.bug192

trait Monoid[X] {
  def id: X
}

class Test extends Monoid[Int => Int] {
  // The problem: miniboxing generates two versions of the
  // id method, one returning a `MiniboxedFunction1` and 
  // another one returning a `Function1`. During erasure,
  // both methods override `def id: X` from `Monoid`, so
  // two bridges are created, both taking object and
  // returning object. We don't want that, so we mark all
  // methods containing `@mbFunction` as artifacts so they
  // don't get bridges. Therefore, after erasure, this 
  // the `Test` class should have 3 versions of method `id`:
  //  * one taking a `MiniboxedFunction1`
  //  * another one taking a `Function1`, redirecting to the first
  //  * another one taking an `Object`, redirecting to the 2nd `id`
  // this should solve the original "duplicate method" error
  // that the JVM was running into
  def id = (x: Int) => x
}
