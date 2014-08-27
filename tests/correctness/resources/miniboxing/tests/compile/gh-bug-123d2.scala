package miniboxing.tests.compile.bug123d
import scala.reflect.ClassTag

class Worker[@miniboxed Id: ClassTag] {

  case class X[T](x: T, edge: Any)

  def receive = (x: Any) => x match {
    case X(source: Id, edge) =>
      sth(source)
  }

  def sth(source: Id) = ???
}
