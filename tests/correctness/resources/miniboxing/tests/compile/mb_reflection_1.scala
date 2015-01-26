package miniboxing.tests.compile.bug171.test1

import MbReflection._

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
