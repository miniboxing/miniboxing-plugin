import plugin.minispec

@minispec 
class Test[T] {
  val five = 5
  val amount = five / 0
  def main(args: Array[String]) {
    println(amount)
  }
}
