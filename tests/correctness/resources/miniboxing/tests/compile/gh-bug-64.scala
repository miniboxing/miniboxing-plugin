package miniboxing.tests.compile.bug64

class A[@miniboxed T]
class C[@miniboxed T] {
  // this statement is not specialized:
  println(new A[T].getClass.getSimpleName)
}
object Test extends App {
  // if the constructor target has been
  // specialized, this should print A_J
  // if not, this will print A_L.
  new C[Int]
}
