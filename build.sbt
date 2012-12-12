name := "miniboxing"

version := "0.1"

scalaVersion := "2.10.0-SNAPSHOT"

//http://stackoverflow.com/questions/10472840/how-to-attach-sources-to-sbt-managed-dependencies-in-scala-ide#answer-11683728
EclipseKeys.withSource := true

compileOrder := CompileOrder.JavaThenScala
