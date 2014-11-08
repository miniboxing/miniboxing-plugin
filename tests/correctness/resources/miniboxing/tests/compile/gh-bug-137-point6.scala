package miniboxing.tests.compile.bug137.point6

class C[@miniboxed T, @miniboxed U](val u: T) {
  class D {
//^ warning: Class D will not be miniboxed based on type parameters T, U of miniboxed class C. To have it specialized, give it two type parameters, marked with @miniboxed and instantiate it using the parameters from class C. 
    val t: T = u
  }
}
