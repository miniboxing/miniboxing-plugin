package miniboxing.tests.bug184

class ConsListSpec {

  test {
    object Apple
  }

  def test(f: => Unit) = ???
}
