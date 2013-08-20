import sbt._
import Keys._
import Process._
import sbtassembly.Plugin._
import AssemblyKeys._

object MiniboxingBuild extends Build {

  val scalaVer = "2.10.3-SNAPSHOT"

  // http://stackoverflow.com/questions/6506377/how-to-get-list-of-dependency-jars-from-an-sbt-0-10-0-project
  val getJars = TaskKey[Unit]("get-jars")
  val getJarsTask = getJars <<= (target, fullClasspath in Runtime) map { (target, cp) =>
    println("Target path is: "+target)
    println("Full classpath is: "+cp.map(_.data).mkString(":"))
  }

  val defaults = Defaults.defaultSettings ++ assemblySettings ++ Seq(
    scalaVersion := scalaVer,
    scalaBinaryVersion := "2.10",
    scalaSource in Compile <<= baseDirectory(_ / "src"),
    javaSource in Compile <<= baseDirectory(_ / "src"),
    scalaSource in Test <<= baseDirectory(_ / "test"),
    javaSource in Test <<= baseDirectory(_ / "test"),
    resourceDirectory in Compile <<= baseDirectory(_ / "resources"),
    compileOrder := CompileOrder.JavaThenScala,

    unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_)),
    //http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide#answer-11683728
    com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys.withSource := true,

    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
      "com.novocode" % "junit-interface" % "0.10-M2" % "test"
    ),

    parallelExecution in Test := false,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),

    // this should work but it doesn't:
    // resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    // so I replaced it with this, which works with both sbt 0.11 and 0.12 
    resolvers in ThisBuild ++= Seq(
      ScalaToolsSnapshots,
      "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
    ),

    // common info to all projects
    version := "0.1-SNAPSHOT",
    organization := "org.scala-miniboxing",
    licenses := Seq("BSD-style" -> url("http://www.scala-lang.org/license.html")),
    homepage := Some(url("http://scala-miniboxing.org"))
  )


  val publishDeps = Seq(
    // sonatype
    publishMavenStyle := true,
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    // pomIncludeRepository := { _ => false },
    pomExtra := (
      <scm>
        <url>git@github.com:miniboxing/miniboxing-plugin.git</url>
        <connection>scm:git:git@github.com:miniboxing/miniboxing-plugin.git</connection>
      </scm>
      <developers>
        <developer>
          <id>VladUreche</id>
          <name>Vlad Ureche</name>
          <url>http://vladureche.ro</url>
        </developer>
      </developers>)
  )

  val nopublishDeps = Seq(
    publish := { }, 
    publishLocal := { }
  )

  val runtimeDeps = Seq(
    scalacOptions ++= Seq("-optimize", "-Yinline-warnings")
  )

  val pluginDeps = Seq(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-library" % scalaVer,
      "org.scala-lang" % "scala-reflect" % scalaVer,
      "org.scala-lang" % "scala-compiler" % scalaVer,
      "org.scala-lang" % "scala-partest" % scalaVer, 
      "com.googlecode.java-diff-utils" % "diffutils" % "1.2.1"
    )
  )

  val classloaderDeps = Seq(
    libraryDependencies ++= Seq(
      "org.ow2.asm" % "asm" % "4.0",
      "org.ow2.asm" % "asm-tree" % "4.0",
      "org.ow2.asm" % "asm-util" % "4.0",
      "org.ow2.asm" % "asm-analysis" % "4.0"
    )
  )

  val scalaMeter = {
    val sMeter  = Seq("com.github.axel22" %% "scalameter" % "0.3")
    // val sMeter  = Seq("com.github.axel22" % "scalameter_2.10.0" % "0.2.1-SNAPSHOT") // published locally
    Seq(
      libraryDependencies ++= sMeter, 
      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
    )
  }

  val testsDeps = Seq(
    getJarsTask,
    fork in Test := true,
    javaOptions in Test <+= (dependencyClasspath in Runtime) map { path =>
      def isBoot(file: java.io.File) = 
        ((file.getName() startsWith "scala-") && (file.getName() endsWith ".jar")) ||
        (file.toString contains "target/scala-2.10") // this makes me cry, seriously sbt...

      val cp = "-Xbootclasspath/a:"+path.map(_.data).filter(isBoot).mkString(":")
      // println(cp)
      cp
    }
  )

  lazy val _mboxing    = Project(id = "miniboxing",             base = file("."),                      settings = defaults ++ nopublishDeps) aggregate (runtime, plugin, classloader, tests, benchmarks)
  lazy val runtime     = Project(id = "miniboxing-runtime",     base = file("components/runtime"),     settings = defaults ++ publishDeps)
  lazy val plugin      = Project(id = "miniboxing-plugin",      base = file("components/plugin"),      settings = defaults ++ publishDeps ++ pluginDeps) dependsOn(runtime)
  lazy val classloader = Project(id = "miniboxing-classloader", base = file("components/classloader"), settings = defaults ++ nopublishDeps ++ classloaderDeps)
  lazy val tests       = Project(id = "miniboxing-tests",       base = file("tests/correctness"),      settings = defaults ++ nopublishDeps ++ classloaderDeps ++ pluginDeps ++ testsDeps) dependsOn(plugin, runtime, classloader)
  lazy val benchmarks  = Project(id = "miniboxing-benchmarks",  base = file("tests/benchmarks"),       settings = defaults ++ nopublishDeps ++ classloaderDeps ++ runtimeDeps ++ scalaMeter) dependsOn(plugin, runtime, classloader)
  lazy val lib_bench   = Project(id = "miniboxing-lib-bench",   base = file("tests/lib-bench"),        settings = defaults ++ nopublishDeps ++ scalaMeter) dependsOn (plugin, runtime)
}
