package correctness
import miniboxing.plugin.minispec

/* This code tests the order of replacing symbols - it should generate valid code */
class Test2[@minispec T] {
  private[this] val t: T = ???
  def foo() = {
    // t should be noted as a miniboxed value
    t.hashCode
  }
}
