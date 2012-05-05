package simple

import plugin.minispec 
import scala.util.Random

class Base

@minispec
trait SpecTrait[@minispec A, @minispec B]


@minispec
class Foo[@minispec T: Manifest] extends Base with SpecTrait[T, T]

@minispec
class Bar[@minispec T: Manifest] extends Foo[T]

class Top extends Bar[Int] {
  def method = {
    val x : Foo[_] = if (Random.nextInt == 0) new Bar[Double] else new Top 
    x
  }
}

