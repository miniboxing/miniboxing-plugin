package miniboxing.classloader.test

import java.io.PrintStream
import java.io.ByteArrayOutputStream

trait Base {

  final def debug(log: String) = System.err.println("[test] " + log)

  def collectSystemOut[T](f: => Unit): String = {
    val osout = System.out
    // http://stackoverflow.com/questions/1760654/java-printstream-to-string
    val barray = new ByteArrayOutputStream()
    System.setOut(new PrintStream(barray))
    System.out.flush()
    val evalf = f
    System.setOut(osout)
    barray.toString()
  }

  def assertEqualString(s1: String, s2: String) =
    assert(s1 == s2, s""""${s1.replaceAll("\n","")}" != "${s2.replaceAll("\n","")}"""")
}
