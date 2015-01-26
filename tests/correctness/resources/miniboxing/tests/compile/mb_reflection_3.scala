package miniboxing.tests.compile.bug171.test3

import MbReflection._

object Test {
  class C[@miniboxed T] {
    println(isMiniboxed[T])
    println(reifiedType[T])
    println(storageType[T])
  }

  class D[T] {
    println(isMiniboxed[T])
    println(reifiedType[T])
    println(storageType[T])
  }

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
