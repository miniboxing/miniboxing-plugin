package miniboxing.tests.compile.bug69.full
 
object Test {
  def methodPlain() = {
    def methodPlain() = ???
    def methodMbox[@miniboxed T](t: T): T = t
    class ClassPlain
    class ClazzMbox[@miniboxed T]
    ???
  }

  def methodMbox[@miniboxed Z](z: Z) = {
    def methodPlain() = ???
    def methodMbox[@miniboxed T](t: T): T = t
    class ClassPlain
    class ClazzMbox[@miniboxed T]
    ???
  }

  class ClassPlain {
    def methodPlain() = ???
    def methodMbox[@miniboxed T](t: T): T = t
    class ClassPlain
    class ClazzMbox[@miniboxed T]
  }


  class ClassMbox[@miniboxed Z] {
    def methodPlain() = ???
    def methodMbox[@miniboxed T](t: T): T = t
    def methodMboxMbox[@miniboxed T](t: T, z: Z): T = t
    class ClassPlain
    class ClazzMbox[@miniboxed T]
  }
}
