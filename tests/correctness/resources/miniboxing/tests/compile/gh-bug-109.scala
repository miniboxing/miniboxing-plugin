package miniboxing.example

trait Growable[@miniboxed -T] {
  
  def ++= (xs: Traversable[T]) = {
    def loop(xs: LinearSeqOptimized[T]) = ???
    xs match {
      case xs: LinearSeqOptimized[_] => loop(xs)
      case xs                        => ???
    }
  }
}

trait Traversable[@miniboxed +T]

trait LinearSeqOptimized[@miniboxed +A] extends Traversable[A]
