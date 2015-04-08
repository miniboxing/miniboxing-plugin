import MbReflection._
import SimpleType._
import SimpleConv._

// more context:
// https://groups.google.com/forum/#!topic/scala-internals/w2BAhTQ59jY

object Test {
  def bippy[@miniboxed A, @miniboxed B](a: A, b: B): B = 
    (reifiedType[A], reifiedType[B]) match {
      case (`int`, `int`) => (a.as[Int] + b.as[Int]).as[B]
      case (  _  , `int`) => (b.as[Int] + 1).as[B]
      case (`int`,   _  ) =>  b
      case (  _  ,   _  ) =>  b
    }

  def main(args: Array[String]): Unit = {
    def x = 1.0
    assert(bippy(3,4) == 7)
    assert(bippy(x,4) == 5)
    assert(bippy(3,x) == x)
    assert(bippy(x,x) == x)
  }
}
