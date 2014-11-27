package miniboxing.tests.compile.bug147
package com.clarifi.miniboxing.test

object Blah {
  def mixie[T,O](proj: List[T => O], ts: T) =
    proj.map(f => f(ts))

  def mixie2[T](proj: List[T => T]) =
    proj.map(f => f)
}

