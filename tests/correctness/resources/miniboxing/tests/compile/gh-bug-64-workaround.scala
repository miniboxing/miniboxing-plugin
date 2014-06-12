package miniboxing.tests.compile.bug64

class A[@miniboxed T]
class C[@miniboxed T] {
  // to solve the problem: put constructor code
  // into an init() call
  def init() {
    println(new A[T].getClass.getSimpleName)
  }
  // don't worry about the call to init() not being
  // specialized, the method body itself will be
  // specialized
  init()
}
object Test extends App {
  // if the constructor target has been
  // specialized, this should print A_J
  // if not, this will print A_L.
  new C[Int]
}
