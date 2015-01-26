package miniboxing.tests.compile.bug171.test4

import MbReflection._

object Test {

  trait T
  trait W

  println(isMiniboxed[T forSome { type T <: String }])
  println(reifiedType[List[Int]])
  println(storageType[T with W])

  println(isMiniboxed[T { def boo: Int } ])
  println(reifiedType[{def baz: String }])
  println(storageType[Any])
} 
