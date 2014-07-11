package miniboxing.tests.compile.spec.interop.functions2

object Fuctions2 {
  def main(args: Array[String]): Unit = {
    val f1 = (x: Int) => x
    f1(4)
    val f2 = foo
    f2(3)
    val fl = List(f2)
    fl(0)(3)
  }

  def foo: (Int => Int) =
    (x: Int) => x
}
