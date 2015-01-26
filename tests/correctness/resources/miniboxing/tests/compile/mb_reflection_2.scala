package miniboxing.tests.compile.bug171.test2

import MbReflection._

object Test {
  def foo[@miniboxed T] = {
    println(isMiniboxed[T])
    println(reifiedType[T])
    println(storageType[T])
  }

  def bar[T] = {
    println(isMiniboxed[T])
    println(reifiedType[T])
   println(storageType[T])
  }
} 
