/** Trait inheritance is a bit tricky because of the
 *  specialized type tags, which need to transform into
 *  getters and need to be overridden. */
package miniboxing.tests.compile.inheritance.traits


trait CC[@miniboxed T] {
  def foo(t: T): T = ???
}

trait DD[@miniboxed T] extends CC[T]

class EE[@miniboxed T] extends DD[T]

class FF extends DD[Int]
