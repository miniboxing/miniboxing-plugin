package miniboxing.tests.compile.bug161

import java.io._

object Test {

  def main(args: Array[String]): Unit = {
    val f = (x: Int) => x + 1

    val pout = new PipedOutputStream()
    val pin = new PipedInputStream(pout)

    val out = new ObjectOutputStream(pout)
    out.writeObject(f)
    out.close()

    val in = new ObjectInputStream(pin)
    in.readObject()
    in.close()

    println("OK")
  }
}
