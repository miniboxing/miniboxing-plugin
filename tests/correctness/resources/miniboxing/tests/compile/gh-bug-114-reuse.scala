package miniboxing.tests.compile.bug114.reuse

object Test {
  def getId(o: Any) =
    System.identityHashCode(o)

  def main(args: Array[String]): Unit = {
    val f = (x: Int) => x
    val g = identity(f)
    val h = identity(g)
    val idf = getId(f)
    val idg = getId(g)
    val idh = getId(h)
    println(idf == idg)
    println(idf == idh)
  }
}
