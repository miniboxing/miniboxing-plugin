package miniboxing.tests.compile.library8


trait Growable[@miniboxed A] {
  def ++=() = {}
}

class VectorBuilder[@miniboxed A] extends Growable[A] {
  override def ++=() =
    super.++=()
}
