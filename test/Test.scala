import plugin.minispec

@minispec
class C[T](val a: T) {
  var z: T = sys.error("hello!")
  def foo(x: T): T = x
}

//@minispec 
//class TestGeneric[T] {
//  val five = 5
//  val amount = five / 0
//  def main(args: Array[String]) {
//    println(amount)
//  }
//}
//
//@minispec 
//class TestStandard {
//  val five = 5
//  val amount = five / 0
//  def main(args: Array[String]) {
//    println(amount)
//  }
//}
//
//@minispec 
//trait TestTraitGeneric[T] {
//  val five = 5
//  val amount = five / 0
//  def main(args: Array[String]) {
//    println(amount)
//  }
//}
//
//@minispec 
//trait TestTraitStandard {
//  val five = 5
//  val amount = five / 0
//  def main(args: Array[String]) {
//    println(amount)
//  }
//}
