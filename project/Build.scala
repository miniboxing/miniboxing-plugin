import sbt._
import Keys._
import Process._

object MiniboxingBuild extends Build {

  val defaults = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.10.0-SNAPSHOT",
    scalaSource in Compile <<= baseDirectory(_ / "src"),
    scalaSource in Test <<= baseDirectory(_ / "test"),
    scalacOptions += "-optimize",
    //fork in Test := true,
    resourceDirectory in Compile <<= baseDirectory(_ / "resources"),
    unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_)),
    // this should work but it doesn't:
    // resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    // so I replaced it with this, which works with both sbt 0.11 and 0.12 :)
    resolvers in ThisBuild += ScalaToolsSnapshots,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-library" % "2.10.0-SNAPSHOT",
      "org.scala-lang" % "scala-compiler" % "2.10.0-SNAPSHOT"
    ),
    parallelExecution in Test := false
  )

  val asmDeps = Seq("org.ow2.asm" % "asm" % "4.0",
                    "org.ow2.asm" % "asm-tree" % "4.0")
  val sMeter  = Seq("com.github.axel22" %% "scalameter" % "0.2")

  val testSettings = Seq(libraryDependencies ++= sMeter, testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"))

  lazy val _mboxing    = Project(id = "miniboxing",             base = file(".")) aggregate (runtime, plugin, classloader, tests)
  lazy val runtime     = Project(id = "miniboxing-runtime",     base = file("components/runtime"),     settings = defaults)
  lazy val plugin      = Project(id = "miniboxing-plugin",      base = file("components/plugin"),      settings = defaults) dependsOn(runtime)
  lazy val classloader = Project(id = "miniboxing-classloader", base = file("components/classloader"), settings = defaults ++ Seq(libraryDependencies ++= asmDeps))
  lazy val tests       = Project(id = "miniboxing-tests",       base = file("tests"),                  settings = defaults ++ testSettings) dependsOn(plugin) dependsOn(runtime) dependsOn(classloader)
}
