
name := "miniboxing"

scalaVersion in Global := "2.10.4"

crossScalaVersions in Global := Seq("2.10.2", "2.10.3", "2.10.4", "2.11.0", "2.11.1")

version in Global := "0.2-SNAPSHOT"

organization in Global := "org.scala-miniboxing.plugins"

organizationName in Global := "LAMP/EPFL"

organizationHomepage in Global := Some(url("http://lamp.epfl.ch"))

licenses in Global := Seq("BSD-style" -> url("http://scala-miniboxing.org/license.html"))

homepage in Global := Some(url("http://scala-miniboxing.org"))

parallelExecution in Global := false
