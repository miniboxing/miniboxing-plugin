package miniboxing.tests.compile.bug113

object Test {
  def arg(x: => Any): Unit = println(x)
  arg(Test.this.getClass())
}
