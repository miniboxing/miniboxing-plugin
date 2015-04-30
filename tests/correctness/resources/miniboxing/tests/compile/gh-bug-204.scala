import scala.util.control.Exception._

class MyCatch[+T](
  pf: Catcher[T],
  fin: Option[Finally] = None,
  override val rethrow: Throwable => Boolean = shouldRethrow)
extends Catch(pf, fin, rethrow)

