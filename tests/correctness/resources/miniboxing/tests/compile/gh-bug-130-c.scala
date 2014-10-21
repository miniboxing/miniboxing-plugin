package miniboxing.tests.compile.bug130c
import language.existentials

object Test {
  class C[@miniboxed T] 

  // no warning, the type argument is a subtype of AnyRef:
  new C[String] // => unspecialized C_L

  // warning, the type parameter is not a subtype of AnyRef, but it's not miniboxed and is not a primitive:
  new C[T forSome { type T <: Int }] // => unspecialized C_L

  // no warning, the type argument is specialized:
  new C[Int] // => specialized C_J
}
