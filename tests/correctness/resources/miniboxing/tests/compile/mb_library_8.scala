package miniboxing.tests.compile.library8

// https://github.com/miniboxing/miniboxing-plugin/issues/45

trait Growable[@miniboxed A] {
  def ++=() = {}
}

class VectorBuilder[@miniboxed A] extends Growable[A] {
  override def ++=() =
    super.++=()
}
