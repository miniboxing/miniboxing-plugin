package miniboxing.tests.compile
import miniboxing.plugin.minispec

trait ComplexAlgebra[@minispec A] {
  def foo: Int
}

trait ComplexInstances {
  // NOTE: The treecheckers error referring to
  // this tree is flat wrong, I checked it.
  implicit def ComplexAlgebra[@minispec A] =
    new ComplexAlgebra[A] {
      def foo = 1
    }
}
