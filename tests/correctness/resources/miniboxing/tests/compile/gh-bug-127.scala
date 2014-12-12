package miniboxing.tests.compile.bug127

class Config {
  override def toString = "a configuration"
}

class Graph[@miniboxed Id](b: Config) {
  println(b)
}

object Test {
  def main(args: Array[String]): Unit = {
    new Graph[Int](new Config)
  }
}
