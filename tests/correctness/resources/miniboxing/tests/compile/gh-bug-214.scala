import scala.reflect._

object Test {
  def baz[@miniboxed T : ClassTag](t: T) = {
    new Array[T](42) // should warn
    Array[T](t,t,t,t,t) // should warn
  }
  def bar[T : ClassTag](t: T) = {
    new Array[T](42)
    Array[T](t,t,t,t,t)
    new Array[Int](42)
  }

  def foo[@miniboxed T](t: T) = {
    MbArray[T](t, t, t)
  }

  def main(args: Array[String]): Unit = {

    println(MbArray[Int](100,200,300).getClass)
    println(MbArray[Double](1.0,2.0,3.0).getClass)
    println(MbArray[String]("1", "2", "3").getClass)
    println(MbArray[Boolean](true,false,true).getClass)
    println(MbArray[Char]('c', 'a', 'z').getClass)

    println(foo[Int](13).getClass)
    println(foo[Long](1).getClass)
    println(foo[Char](1).getClass)
    println(foo[Short](1).getClass)
    println(foo[Byte](1).getClass)
    println(foo[Boolean](true).getClass)
    println(foo[Double](1.0).getClass)
    println(foo[String]("string").getClass)

    ()
  }
}