package miniboxing.infrastructure

import org.junit.Test
import java.io.{ File => JFile }
import scala.tools.nsc.io._
import scala.tools.partest.nest.FileUtil._
import java.io.FileNotFoundException

/* Taken from: [[https://github.com/nicolasstucki/specialized/commit/f7ee90610d0052cb3607cef138051575db3c2eb9]] */
class TestSuite {

  private[this] def files(dirs: List[String], ext: String) = {
    val cwd = sys.props.get("user.dir").getOrElse(".")
    val res = dirs.foldLeft(Path(new JFile(cwd)))((path, dir) => path / dir)
    System.err.println("Picking tests from: " + res)
    res.jfile.listFiles().filter(_.getName().endsWith(ext))
  }

  private[this] def replaceExtension(source: JFile, ext: String) =
    new JFile(source.toString.replaceAll("\\.scala", "." + ext))

  private[this] def slurp(source: JFile) =
    try {
      File(replaceExtension(source, "flags")).slurp
    } catch {
      case _: FileNotFoundException => ""
    }

  // TODO: This needs to be general, it's currently a mess
  private[this] def pluginCompilerFlag() =
    "-Xplugin:" + files(List("components", "plugin", "target", "scala-2.10"), ".jar").head.toString

  @Test def testCompileOutput = {
    var failed = false
    val pluginFlag = pluginCompilerFlag()

    for (source <- files(List("tests", "correctness", "src", "miniboxing", "tests", "compile"), ".scala")) {
      System.err.println(s"Running ${source.toString}")

      // source code:
      val code =       File(source).slurp
      val flags =      pluginFlag + " " + slurp(replaceExtension(source, "flags"))
      val exp_output = slurp(replaceExtension(source, "check"))
      val output = new CompileTest(code, flags).compilationOutput()
      // this stopped working due to incorrect deps: compareContents(output.split("\n"), exp_output.split("\n"))
      val diff = output != exp_output

      if (diff) {
         System.err.println("\n\n\nDifference in test for: " + source)
         System.err.println("\nCompiler output:\n" + output)
         System.err.println("\nExpected output:\n" + exp_output)
         failed = true
      }
    }

    assert(!failed, "Some tests failed.")
  }
}
