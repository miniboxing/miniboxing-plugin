package miniboxing.tests.compile.library8
import miniboxing.plugin.minispec

trait Growable[@minispec A] {
  def ++=() = {}
}

class VectorBuilder[@minispec A] extends Growable[A] {
  override def ++=() =
    super.++=()
}
