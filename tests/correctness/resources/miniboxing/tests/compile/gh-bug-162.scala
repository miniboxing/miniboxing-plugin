package miniboxing.tests.compile.bug162

trait BaseVectorGenerator[A]

class WordSpec
abstract class VectorSpec[@miniboxed A] extends WordSpec      with BaseVectorGenerator[A]
abstract class VectorTest[@miniboxed A] extends VectorSpec[A] with BaseVectorGenerator[A]
