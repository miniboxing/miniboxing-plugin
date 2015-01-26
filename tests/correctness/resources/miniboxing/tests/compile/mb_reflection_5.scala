package miniboxing.tests.compile.bug171.test5

import MbReflection._

object Test {

  println(isMiniboxed[Int])
  println(reifiedType[Int])
  println(storageType[Int])

  println(isMiniboxed[Float])
  println(reifiedType[Float])
  println(storageType[Float])

} 
