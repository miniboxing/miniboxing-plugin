import plugin.minispec

class Test[@specialized T] {
  val five = 5
  val amount = five / 0
  def main(args: Array[String]) {
    println(amount)
  }
}
