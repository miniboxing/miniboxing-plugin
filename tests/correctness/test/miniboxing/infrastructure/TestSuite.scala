package miniboxing.infrastructure

import org.junit.Test
import java.io.{ File => JFile }
import scala.tools.nsc.io._
import scala.tools.partest.nest.FileUtil._
import java.io.FileNotFoundException
import difflib._;

/* Taken from: [[https://github.com/nicolasstucki/specialized/commit/f7ee90610d0052cb3607cef138051575db3c2eb9]] */
class TestSuite {

  private[this] def files(dirs: List[String], ext: String) = {
    val cwd = sys.props.get("user.dir").getOrElse(".")
    val res = dirs.foldLeft(Path(new JFile(cwd)))((path, dir) => path / dir)
    System.err.println("Picking tests from: " + res)
    res.jfile.listFiles().filter(_.getName().endsWith(ext)).sortBy(_.getName())
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
    try {
      "-Xplugin:" + files(List("components", "plugin", "target", "scala-2.10"), ".jar").head.toString
    } catch {
      case x: NoSuchElementException =>
        sys.error("The plugin jar is not available! Run \"sbt miniboxing-plugin/package\" to generate it.")
    }

  @Test def testCompileOutput = {
    var failed = false
    val pluginFlag = pluginCompilerFlag()

    for (source <- files(List("tests", "correctness", "src", "miniboxing", "tests", "compile"), ".scala")) {
      System.err.println(s"Running ${source.toString}")

      // source code:
      val code =       File(source).slurp
      val flags =      pluginFlag + " " + slurp(replaceExtension(source, "flags"))
      val expect = slurp(replaceExtension(source, "check"))
      val output = new CompileTest(code, flags).compilationOutput()
      import scala.collection.JavaConversions._
      val output_lines = asJavaList(output.split("\n").toList)
      val expect_lines = asJavaList(expect.split("\n").toList)
      val sdiff = DiffUtils.diff(expect_lines, output_lines)
      val udiff = DiffUtils.generateUnifiedDiff("output", "expected", expect_lines, sdiff, 2)

      if (sdiff.getDeltas().size() != 1) {
        System.err.println("\n\n\nDifference in test for: " + source)
        System.err.println("\nDiff: ")
        for (line <- udiff)
          System.err.println(line)
        System.err.println("\nCompiler output:\n" + output)
        System.err.println("\nExpected output:\n" + expect)
        failed = true
      }
    }

    assert(!failed, "Some tests failed.")
  }
}
