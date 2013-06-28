/** Trait inheritance is a bit tricky because of the
 *  specialized type tags, which need to transform into
 *  getters and need to be overridden. */
package miniboxing.tests.compile.inheritance.traits
import miniboxing.plugin.minispec

trait CC[@minispec T] {
  def foo(t: T): T = ???
}

trait DD[@minispec T] extends CC[T]

class EE[@minispec T] extends DD[T]

class FF extends DD[Int]
