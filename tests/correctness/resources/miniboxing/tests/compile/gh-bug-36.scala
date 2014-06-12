package miniboxing.tests.compile.bug36

// https://github.com/miniboxing/miniboxing-plugin/issues/36

trait VectorLike[@miniboxed E, +Self <: Vector[E]]
trait Vector[E] extends VectorLike[E, Vector[E]]

