package miniboxing.tests.compile.bug162d

class A(i: Int)
class B[@miniboxed T](t: T) extends A(3)
