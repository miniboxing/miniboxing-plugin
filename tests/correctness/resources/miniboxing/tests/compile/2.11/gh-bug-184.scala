package miniboxing.tests.bug184

class ConsListSpec {

  test {
    case object Apple
  }

  def test(f: => Unit) = ???
}
