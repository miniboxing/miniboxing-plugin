package miniboxing.infrastructure

import org.junit.Test
import java.io.{ File => JFile }
import scala.tools.nsc.io._
import java.io.FileNotFoundException
import difflib._
import java.io.PrintWriter
import miniboxing.plugin.ScalacVersion
import difflib.myers._

/* Taken from: [[https://github.com/nicolasstucki/specialized/commit/f7ee90610d0052cb3607cef138051575db3c2eb9]] */
class TestSuite extends ScalacVersion {

  val scalaPrinterCompatibility = List[(String, String)](
    " >: Nothing" -> "",
    " <: Any" -> "",
    "warning: 'minibox' selects 4 phases\n" -> "",
    "warning: 'inject' selects 4 phases\n" -> "",
    "<\\$anon: .*?>" -> "anonymous class \\$anon",
    " @scala.annotation.unchecked.uncheckedVariance" -> "",
    "Int\\([0-9]*?\\)" -> "Int",
    "<artifact> " -> "",
    "\\+([A-Z])" -> "$1",
    "\\-([A-Z])" -> "$1",
    "warning: \\[check:" -> "\\[check:",
    "\n(?s)Out of scope symbol reference.*?\n}\n" -> " symbol out of scope\n",
    "The symbol, tpe or info of tree.*?\n" -> "symbol out of scope\n",
    "warning: TreeCheckers detected non-compliant trees in newSource1.scala" -> "",
    "warning: Reference to uninitialized variable t" -> "", // TODO: Remove after looking at #104
    "\\$anon .*?>" -> "\\$anon",
    "@SerialVersionUID\\(value = " -> "@SerialVersionUID(",
    "anonymous class \\$anonfun" -> "anonymous class \\$anon",
    "def .*\\$\\$outer\\(\\)" -> "def \\$outer()",
    "@miniboxing.runtime.nobridge " -> "",
    // for mb_array_03.scala:
    "\\(ClassTag.apply\\[String\\]\\(classOf\\[java.lang.String\\]\\): scala\\.reflect\\.ClassTag\\[String\\]\\)" -> "ClassTag.apply[String](classOf[java.lang.String])",
    // for mb_array_04.scala:
    "\\(ClassTag.apply\\[F\\]\\(classOf\\[java.lang.Object\\]\\): scala.reflect.ClassTag\\[F\\]" -> "ClassTag.apply[F](classOf[java.lang.Object]",
    // REPL leftovers:
    "Type in expressions to have them evaluated." -> "",
    "Type :help for more information." -> "",
    "scala> :quit" -> "",
    "scala> $" -> "",
    // random object addresses:
    "@[0-9a-f]{1,8}" -> "@<object_id>"
  )

  implicit class JFileExt(jfile: JFile) {
    def listFilesNotNull(): List[JFile] = {
      val res = jfile.listFiles()
      if (res == null) Nil else res.toList
    }
  }

  private[this] def files(dirs: List[String], exts: String*) = {
    val cwd = sys.props.get("user.dir").getOrElse(".")
    val res = dirs.foldLeft(Path(new JFile(cwd)))((path, dir) => path / dir)
    System.err.println("Picking tests from: " + res)
    res.jfile.listFilesNotNull().filter(file => exts.exists(ext => file.getName().endsWith(ext))).sortBy(_.getName())
  }

  private[this] def replaceExtension(source: JFile, ext: String) =
    new JFile(source.toString.replaceAll("\\.\\p{Alnum}*$", "." + ext))

  private[this] def slurp(source: JFile) =
    try {
      File(source).slurp
    } catch {
      case _: FileNotFoundException => ""
    }

  // TODO: This needs to be general, it's currently a mess
  private[this] def pluginCompilerFlag() =
    try {
      "-Xplugin:" + files(List("..", "..", "components", "plugin", "target", "scala-" + scalaBinaryVersion), ".jar").head.toString
    } catch {
      case x: NoSuchElementException =>
        sys.error("The plugin jar is not available! Run \"sbt miniboxing-plugin/package\" to generate it.")
    }

  @Test def testCompileOutput() = {
    var failed = false
    var totalTests = 0
    var failedTests = 0
    val pluginFlag = pluginCompilerFlag()
    var UPDATE_CHECKFILE = false
    // use carefully:
//    UPDATE_CHECKFILE = true

    sys.props("miniboxing.no-logo") = "true"

    val tests = files(List("resources", "miniboxing", "tests", "compile"), ".scala", ".repl").toList :::
                files(List("resources", "miniboxing", "tests", "compile", scalaBinaryVersion), ".scala", ".repl").toList

    for (source <- tests) {
      System.err.print(f"Compiling ${source.getName()}%-60s ... ")
      val isRepl = source.getName().endsWith(".repl")

      totalTests += 1

      // source code:
      val code = File(source).slurp
      val flags = pluginFlag + " " + slurp(replaceExtension(source, "flags"))
      val check_file = if (!isRepl) replaceExtension(source, "check") else source
      var expect = if (!isRepl) slurp(check_file) else code
      val launch_file = replaceExtension(source, "launch")
      val launch = if (!isRepl) slurp(launch_file).replace("\n","") else ""
      var output =
        isRepl match {
          case true =>
            val prompt = "scala> "
            val code_only = code.lines.filter(_ startsWith prompt).map(_ drop prompt.length).mkString("\n")
            new ReplTest(code_only, flags).replOutput()
          case false =>
            new CompileTest(code, flags, launch).compilationOutput()
        }
      for ((regex, replace) <- scalaPrinterCompatibility) {
        output = output.replaceAll(regex, replace)
        expect = expect.replaceAll(regex, replace)
      }
      import scala.collection.JavaConversions._
      def stripTrailingWS(s: String) = s.replaceAll("\\s*$","")
      val output_lines = seqAsJavaList(output.split("\n").toList.map(stripTrailingWS)).filter(_ != "")
      val expect_lines = seqAsJavaList(expect.split("\n").toList.map(stripTrailingWS)).filter(_ != "")
      val generator = new DiffRowGenerator.Builder().showInlineDiffs(true).ignoreWhiteSpaces(true).columnWidth(100).build()
      val differ = new MyersDiff(new Equalizer[String] {
        def equals(a: String, b: String): Boolean = {
            val _a = a.trim.replaceAll("\\s+", " ")
            val _b = b.trim.replaceAll("\\s+", " ")
            return _a.equals(_b)
        }
      })
      val sdiff = DiffUtils.diff(expect_lines, output_lines, differ)
      val udiff = DiffUtils.generateUnifiedDiff("output", "expected", expect_lines, sdiff, 2)

      if (sdiff.getDeltas().size() != 0) {
        if (UPDATE_CHECKFILE) {
          System.err.println("[ UP ] " + "\n" + check_file + "\n")
          Some(new PrintWriter(check_file)).foreach{p => p.write(output_lines.mkString("\n")); p.close}
        } else
          System.err.println("[FAIL]")
          //System.err.println("\nDifference in test for: " + source)
          System.err.println("Diff: ")
          for (line <- udiff)
            System.err.println(line)
            //System.err.println("\nCompiler output:\n" + output)
            //System.err.println("\nExpected output:\n" + expect)
          System.err.println("\n\n")
          failed = true
          failedTests += 1
      } else
        System.err.println("[ OK ]")
    }

    if (failedTests != 0)
      System.err.println(s"\n  $totalTests tests ran, $failedTests ${if (UPDATE_CHECKFILE) "updated" else "failed"} :(\n")
    else
      System.err.println(s"\n  $totalTests tests ran, all good :)\n")

    assert(!UPDATE_CHECKFILE && !failed, "Some tests failed. (or UPDATE_CHECKFILE is on)")
  }
}
