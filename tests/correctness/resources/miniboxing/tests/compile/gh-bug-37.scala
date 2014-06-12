package miniboxing.tests.compile.bug37

trait Growable[@miniboxed -A] {
  def +=(elem: A): this.type
  def ++=(xs: TraversableOnce[A]): this.type = { xs.seq foreach += ; this }
}
