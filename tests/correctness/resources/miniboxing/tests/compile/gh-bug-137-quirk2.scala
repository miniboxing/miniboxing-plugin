package miniboxing.tests.compile.bug137.quirk2

trait Traversable[@miniboxed T] {
  self =>
}

object Traversable {
  class Buffer[@miniboxed T]
  extends Traversable[T] {
    def +=(value: T) {
    }
  }
}

class Actions[@miniboxed K, V](val stopKey: K) {
  private val insertsBuffer = new Traversable.Buffer[(K, V)]
  private val clearsBuffer = new Traversable.Buffer[Unit]

  def inserts: Traversable[(K, V)] = insertsBuffer
  def clears: Traversable[Unit] = clearsBuffer
}

object Test {
  def main(args: Array[String]): Unit = {
    new Actions(3)
    println("OK")
  }
}
