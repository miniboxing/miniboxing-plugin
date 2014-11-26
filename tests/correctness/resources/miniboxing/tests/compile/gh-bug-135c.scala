package miniboxing.tests.compile.bug135c

class C[U] {
  def foo(): U = ???
}

class D extends C[Int => Int] {
  override def foo(): Int => Int = (x: Int) => x + 1
  // the following method will become:
  //   override def foo: MbFunction1[Int, Int] = ...
  // and it should have 1 bridge:
  //   @bridge  def foo: Int => Int = ...
}

