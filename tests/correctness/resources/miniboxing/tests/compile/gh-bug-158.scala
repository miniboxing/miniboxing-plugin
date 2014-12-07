package miniboxing.tests.compile.bug158

import java.io._

// http://stackoverflow.com/questions/691813/is-there-a-null-outputstream-in-java
class NullOutputStream extends OutputStream {
  def write(b: Int): Unit = ()
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = (x: Int) => x + 1

    val out = new ObjectOutputStream(new NullOutputStream())
    out.writeObject(f)

    println("OK")
  }
}
