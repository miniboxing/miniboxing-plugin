package miniboxing.infrastructure

import scala.tools.partest._
import scala.tools.nsc._
import nest.FileUtil._
import scala.reflect.io._
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.io.{File => JFile}
import java.io.PrintWriter
import java.net.URLClassLoader

/* Taken from: [[https://github.com/nicolasstucki/specialized/commit/f7ee90610d0052cb3607cef138051575db3c2eb9]]
 * Using the Partest tool in Scala:
 * [[https://github.com/scala/scala/blob/master/src/partest/scala/tools/partest/DirectTest.scala]]
 */
class CompileTest(val code: String, flags: String, launch: String) extends DirectTest {

  lazy val jfile = {
    var root: JFile = null
    while (root == null) {
      val rand = scala.util.Random.nextLong()
      val rand_pos = if (rand < 0) -rand else rand
      root = new JFile(System.getProperty("java.io.tmpdir") + "/" + "miniboxing-" + rand_pos)
      if (root.exists() && root.isDirectory())
        root = null
    }
    root.mkdirs()
    root.deleteOnExit()
    root
  }
  override lazy val testPath = File(jfile)
  override lazy val testOutput = Directory(jfile)

  override def extraSettings = flags

  def show() = compilationOutput()
  def compilationOutput(): String = {
    val ba = new ByteArrayOutputStream();
    val pa = new PrintStream(ba)
    val pOut = Console.out
    val pErr = Console.err
    Console.setOut(pa)
    Console.setErr(pa)
    // compile the given file:
    val result = compile()
    // if necessary, reflectively launch the test execution:
    if (launch != "") {
      val args = launch.split(" ")
      val loader = new URLClassLoader(Array(jfile.toURI().toURL()), this.getClass.getClassLoader)
      val clazz = loader.loadClass(args(0))
      val method = clazz.getMethod("main", classOf[Array[String]])
      method.invoke(null, args.drop(1))
    }
    pa.flush()
    Console.setOut(pOut)
    Console.setErr(pErr)
   ba.toString
  }
}
