package miniboxing.tests.compile

object Test2 extends App {
  val a = new MBResizableArray[Int](implicitly[Manifest[Int]])
  a.add(1)
  a.add(2)
  a.add(3)
  a.add(4)
  a.add(5)
  a.add(6)
  a.reverse
  assert(a.contains(3))
}

