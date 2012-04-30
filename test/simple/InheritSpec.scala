package simple

trait SpecTrait[A, B]

class Base 
class Foo[A,B] extends Base with SpecTrait[A, Int] // XXX:  won't compile if add  'with SpecTrait[Double, A]'
class Bar[A,B] extends Foo[A,B]
class Top extends Bar[Int, Double]

class Foo2[A] extends Top

