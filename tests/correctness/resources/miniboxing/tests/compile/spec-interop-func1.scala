package miniboxing.tests.compile.spec.interop.functions1

object Fuctions1 {
  def main(args: Array[String]): Unit = {
    val f1 = (x: Int, y: Int) => x
    val f2 = f1.curried
    val f3 = f2(4)
    val f4 = foo
    println(f3(3)) 
    val fl = List(f2)
  }

  def foo: (Int => Int) =
    (x: Int) => x
}
