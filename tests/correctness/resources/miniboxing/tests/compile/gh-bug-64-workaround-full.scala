package miniboxing.tests.compile.bug64.workaround

class A[@miniboxed T](val t: T) {
  println("we want a warning for this!")
}

class C[@miniboxed T](val t: T) {
  println(new A(t).getClass())
}

class D[@miniboxed T](val t: T) {
  def init() = {
    println(new A(t).getClass())
  }
  init()
}

object Test extends App {
  new C(123)
  new C(1.3)
  new C("x")
  new D(123)
  new D(1.3)
  new D("x")
}
