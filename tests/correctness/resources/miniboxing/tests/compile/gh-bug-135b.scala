package miniboxing.tests.compile.bug135b

class C[U,V] {
  def foo(u: U, v: V) = true
}

class D[U] extends C[U, Int => Int] {
  override def foo(u: U, v: Int => Int) = false
  // the following method will become:
  //   override def foo(u: U, v: MbFunction1[Int, Int]) = true
  // and it should have 1 bridge:
  //   @bridge  def foo(u: U, v: Int => Int) = ...
}

class E extends D[String => String] {
  override def foo(u: String => String, v: Int => Int) = true
  // the following method will become:
  //   override def foo(u: MbFunction1[String, String], v: MbFunction1[Int, Int]) = true
  // and it should have 2 bridges:
  //   @bridge  def foo(u: String => String, v: MbFunction1[Int, Int]) = ...
  //   @bridge  def foo(u: String => String, v: Int => Int) = ...
}
