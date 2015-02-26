//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
import sbt._
import Keys._
import Process._
import sbtassembly.Plugin._
import AssemblyKeys._
import xerial.sbt.Sonatype._ 
import SonatypeKeys._

object MiniboxingBuild extends Build {

  // http://stackoverflow.com/questions/6506377/how-to-get-list-of-dependency-jars-from-an-sbt-0-10-0-project
  val getJars = TaskKey[Unit]("get-jars")
  val getJarsTask = getJars <<= (target, fullClasspath in Runtime) map { (target, cp) =>
    println("Target path is: "+target)
    println("Full classpath is: "+cp.map(_.data).mkString(":"))
  }

  val defaults = Defaults.defaultSettings ++ assemblySettings ++ Seq(
    scalaSource in Compile := baseDirectory.value / "src",
    javaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",
    javaSource in Test := baseDirectory.value / "test",
    resourceDirectory in Compile := baseDirectory.value / "resources",
    compileOrder := CompileOrder.Mixed,

    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value),
    //http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide#answer-11683728
    com.typesafe.sbteclipse.plugin.EclipsePlugin.EclipseKeys.withSource := true,

    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint"),

    parallelExecution in Global := false,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    }
  )

  val crossCompilationLayer = Seq(
    unmanagedSourceDirectories in Compile +=  baseDirectory.value / "cross-compilation" / scalaBinaryVersion.value
  )

  val publishCredFile = "miniboxing.maven.credentials-file"
  val publishRealm = "MINIBOXING_MAVEN_REALM"
  val publishDomain = "MINIBOXING_MAVEN_DOMAIN"
  val publishUser = "MINIBOXING_MAVEN_USER"
  val publishPass = "MINIBOXING_MAVEN_PASS"
  val publishCredAvailable = sys.props.isDefinedAt(publishCredFile) || 
                             sys.env.isDefinedAt(publishRealm) && 
                             sys.env.isDefinedAt(publishDomain) && 
                             sys.env.isDefinedAt(publishUser) && 
                             sys.env.isDefinedAt(publishPass)

  val publishDeps: Seq[Setting[_]] = publishCredAvailable match {
    case true => 
      sonatypeSettings ++ Seq(
        // sonatype
        profileName := "vlad.ureche",
        publishMavenStyle := true,
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
        {
          sys.props.get(publishCredFile) match {
            case Some(credFile) => 
              credentials += Credentials(new java.io.File(credFile))
            case None =>
              credentials += Credentials(sys.env(publishRealm),
                                         sys.env(publishDomain),
                                         sys.env(publishUser),
                                         sys.env(publishPass))
          }
        },
        publishArtifact in packageDoc := !isSnapshot.value
      )
   case false => 
     Seq(
       publish <<= streams.map(_.log.info(s"""Publishing to Sonatype is disabled since neither the "$publishCredFile" nor "$publishRealm"/"$publishDomain"/"$publishUser"/"$publishPass" are set."""))
     )
  }

  val nopublishDeps = Seq(
    publishArtifact := false
  )

  val runtimeDeps = Seq(
    scalacOptions ++= Seq("-optimize", "-Yinline-warnings")
    // libraryDependencies += "com.github.scala-blitz" %% "scala-blitz" % "1.0-M2"
  )

  val pluginDeps = Seq(
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
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
    val scalaMeter  = Seq("com.github.axel22" %% "scalameter" % "0.5-M2")
    val scalaMeterFramework = new TestFramework("org.scalameter.ScalaMeterFramework")
    Seq(
      libraryDependencies ++= scalaMeter, 
      testFrameworks += scalaMeterFramework,
      testOptions in ThisBuild += Tests.Argument(scalaMeterFramework, "-silent", "-preJDK7")
    )
  }

  val junitDeps: Seq[Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.10-M2" % "test"
    ),
    parallelExecution in Test := false,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")
  )

  val pluginCompilationDeps: Seq[Setting[_]] = Seq(
    fork in Test := true,
    scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      //System.setProperty("miniboxing.plugin.jar", jar.getAbsolutePath)
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      // Thanks Jason for this cool idea (taken from https://github.com/retronym/boxer)
      // add plugin timestamp to compiler options to trigger recompile of
      // main after editing the plugin. (Otherwise a 'clean' is needed.)
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    }
  )

  val testsDeps: Seq[Setting[_]] = junitDeps ++ Seq(
    getJarsTask,
    fork in Test := true,
    javaOptions in Test <+= (dependencyClasspath in Runtime, scalaBinaryVersion, packageBin in Compile in plugin) map { (path, ver, _) =>
      def isBoot(file: java.io.File) = 
        ((file.getName() startsWith "scala-") && (file.getName() endsWith ".jar")) ||
        (file.toString contains ("target/scala-" + ver)) // this makes me cry, seriously sbt...

      val cp = "-Xbootclasspath/a:" + path.map(_.data).filter(isBoot).mkString(":")
      // println(cp)
      cp
    },
    libraryDependencies ++= (
      if (scalaVersion.value.startsWith("2.10")) {
        Seq(
          "org.scala-lang" % "scala-partest" % scalaVersion.value, 
          "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0"
        )
      } else {
        Seq(
          "org.scala-lang.modules" %% "scala-partest" % "1.0.0",
          "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0"
        )
      }
    ),
    // I want the ScalaTest library around:
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4"
  )

  val recursiveDeps = {
    val ver = "0.4-SNAPSHOT"
    val bootstrap = sys.props.getOrElse("miniboxing.bootstrap", "no")
    bootstrap match {
      case "stage1" =>
        Seq()
      case _ =>
        Seq(
          libraryDependencies += {
                                   val sVer = scalaBinaryVersion.value
                                   compilerPlugin(
                                     "org.scala-miniboxing.plugins" %% "miniboxing-plugin" % ver from 
                                       s"https://oss.sonatype.org/content/repositories/snapshots/org/scala-miniboxing/plugins/miniboxing-plugin_$sVer/$ver/miniboxing-plugin_$sVer-$ver.jar")
                                 },
          // after publishing the new version:
          // scalacOptions ++= Seq("-P:minibox:Ykeep-functionX-repres") ++ (if (bootstrap == "stage2") Seq("-P:minibox:Ystrip-miniboxed") else Seq())
          scalacOptions ++= Seq("-P:minibox:library-functions", "-P:minibox:warn-off") ++ (if (bootstrap == "stage2") Seq("-P:minibox:Ystrip-miniboxed") else Seq())
      )
    }
  }

  lazy val _mboxing    = Project(id = "miniboxing",             base = file("."),                      settings = defaults ++ nopublishDeps) aggregate (runtime, plugin, classloader, tests, benchmarks)
  lazy val runtime     = Project(id = "miniboxing-runtime",     base = file("components/runtime"),     settings = defaults ++ publishDeps ++ recursiveDeps)
  lazy val plugin      = Project(id = "miniboxing-plugin",      base = file("components/plugin"),      settings = defaults ++ publishDeps ++ pluginDeps ++ crossCompilationLayer) dependsOn(runtime)
  lazy val classloader = Project(id = "miniboxing-classloader", base = file("components/classloader"), settings = defaults ++ nopublishDeps ++ classloaderDeps ++ junitDeps)
  lazy val tests       = Project(id = "miniboxing-tests",       base = file("tests/correctness"),      settings = defaults ++ nopublishDeps ++ classloaderDeps ++ pluginDeps ++ testsDeps) dependsOn(plugin, runtime, classloader)
  lazy val benchmarks  = Project(id = "miniboxing-benchmarks",  base = file("tests/benchmarks"),       settings = defaults ++ nopublishDeps ++ classloaderDeps ++ runtimeDeps ++ scalaMeter) dependsOn(plugin, runtime, classloader)
  lazy val lib_bench   = Project(id = "miniboxing-lib-bench",   base = file("tests/lib-bench"),        settings = defaults ++ nopublishDeps ++ scalaMeter ++ pluginCompilationDeps ++ runtimeDeps) dependsOn (plugin, runtime)
}
