package miniboxing.infrastructure

import scala.tools.partest._
import scala.tools.nsc._
import nest.FileUtil._
import scala.reflect.io._
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.io.{File => JFile}
import java.io.PrintWriter

/* Taken from: [[https://github.com/nicolasstucki/specialized/commit/f7ee90610d0052cb3607cef138051575db3c2eb9]]
 * Using the Partest tool in Scala:
 * [[https://github.com/scala/scala/blob/master/src/partest/scala/tools/partest/DirectTest.scala]]
 */
class CompileTest(val code: String, flags: String) extends DirectTest {

   override lazy val testPath = File(new JFile(System.getProperty("java.io.tmpdir")))
   override lazy val testOutput = Directory(new JFile(System.getProperty("java.io.tmpdir")))

   override def extraSettings = flags

   def show() = compilationOutput()
   def compilationOutput(): String = {
     val ba = new ByteArrayOutputStream();
     val pa = new PrintStream(ba)
     val pOut = Console.out
     val pErr = Console.err
     Console.setOut(pa)
     Console.setErr(pa)
     val result = compile()
     pa.flush()
     Console.setOut(pOut)
     Console.setErr(pErr)
     ba.toString
   }
}
