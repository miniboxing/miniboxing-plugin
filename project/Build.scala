import sbt._
import Keys._
import Process._
import sbtassembly.Plugin._
import AssemblyKeys._

object MiniboxingBuild extends Build {

  val scalaVer = "2.10.4-SNAPSHOT"

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

    resolvers in ThisBuild ++= Seq(
      ScalaToolsSnapshots,
      "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
    )
  )

  val publishCredFile = "miniboxing.maven.credentials-file"
  val publishDeps: Seq[Setting[_]] = sys.props.get(publishCredFile) match {
    case Some(credFile) => 
      Seq(
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
        pomIncludeRepository := { _ => false },
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
          </developers>),
        credentials += Credentials({ new java.io.File(credFile) })
      )
   case None => 
     Seq(
       publish <<= streams.map(_.log.info("Publishing to Sonatype is disabled since the \"" + publishCredFile + "\" variable is not set."))
     )
  }

  val nopublishDeps = Seq(
    publish := { }, 
    publishLocal := { }
  )

  val runtimeDeps = Seq(
    scalacOptions ++= Seq("-optimize", "-Yinline-warnings")
  )

  val pluginDeps = Seq(
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVer
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
    // nightlies!!! https://github.com/axel22/scalameter/pull/33
    val sMeter  = Seq("com.github.axel22" %% "scalameter" % "0.4-SNAPSHOT")
    Seq(
      libraryDependencies ++= sMeter, 
      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
    )
  }

  val junitDeps: Seq[Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
      "com.novocode" % "junit-interface" % "0.10-M2" % "test"
    ),
    parallelExecution in Test := false,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")
  )

  val testsDeps: Seq[Setting[_]] = junitDeps ++ Seq(
    getJarsTask,
    fork in Test := true,
    javaOptions in Test <+= (dependencyClasspath in Runtime, packageBin in Compile in plugin) map { (path, _) =>
      def isBoot(file: java.io.File) = 
        ((file.getName() startsWith "scala-") && (file.getName() endsWith ".jar")) ||
        (file.toString contains "target/scala-2.10") // this makes me cry, seriously sbt...

      val cp = "-Xbootclasspath/a:"+path.map(_.data).filter(isBoot).mkString(":")
      // println(cp)
      cp
    },
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-partest" % scalaVer, 
      "com.googlecode.java-diff-utils" % "diffutils" % "1.2.1"
    )
  )

  lazy val _mboxing    = Project(id = "miniboxing",             base = file("."),                      settings = defaults ++ nopublishDeps) aggregate (runtime, plugin, classloader, tests, benchmarks)
  lazy val runtime     = Project(id = "miniboxing-runtime",     base = file("components/runtime"),     settings = defaults ++ publishDeps)
  lazy val plugin      = Project(id = "miniboxing-plugin",      base = file("components/plugin"),      settings = defaults ++ publishDeps ++ pluginDeps) dependsOn(runtime)
  lazy val classloader = Project(id = "miniboxing-classloader", base = file("components/classloader"), settings = defaults ++ nopublishDeps ++ classloaderDeps ++ junitDeps)
  lazy val tests       = Project(id = "miniboxing-tests",       base = file("tests/correctness"),      settings = defaults ++ nopublishDeps ++ classloaderDeps ++ pluginDeps ++ testsDeps) dependsOn(plugin, runtime, classloader)
  lazy val benchmarks  = Project(id = "miniboxing-benchmarks",  base = file("tests/benchmarks"),       settings = defaults ++ nopublishDeps ++ classloaderDeps ++ runtimeDeps ++ scalaMeter) dependsOn(plugin, runtime, classloader)
  lazy val lib_bench   = Project(id = "miniboxing-lib-bench",   base = file("tests/lib-bench"),        settings = defaults ++ nopublishDeps ++ scalaMeter) dependsOn (plugin, runtime)
}
