package miniboxing.tests.compile.caseclass1

case class C[@miniboxed T](t: T)

object Test extends App {
  println(C(3).getClass.getSimpleName)
}
