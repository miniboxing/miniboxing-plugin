package miniboxing.tests.compile.bug123d
import scala.reflect.ClassTag

class Worker {

// works:
//  def recv[@miniboxed Id](x: Any)(implicit ct: ClassTag[Id]) = {
//    val opt: Option[Id] = ct.unapply(x)
//    val id = opt.get
//  }

  def receive[@miniboxed Id: ClassTag] = (x: Any) => x match {
    case source: Id =>
      val src: Id = source
  }
}
