package miniboxing.tests.compile

// check normalization hijacking
class HijackMe2[@specialized(Int) T] {
  def hijackMeToo[@specialized(Int) U](t: T, u: U) = ???
}
