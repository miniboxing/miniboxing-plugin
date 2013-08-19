package miniboxing.tests.compile


trait ComplexAlgebra[@miniboxed A] {
  def foo: Int
}

trait ComplexInstances {
  // NOTE: The treecheckers error referring to
  // this tree is flat wrong, I checked it.
  implicit def ComplexAlgebra[@miniboxed A] =
    new ComplexAlgebra[A] {
      def foo = 1
    }
}
