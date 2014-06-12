package miniboxing.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.InfoTransform
import scala.tools.nsc.transform.TypingTransformers

trait ScalacCrossCompilingLayer {

  val global: Global

  implicit class RichGlobal(glb: global.type) {
    def beforePhase[T](phase: Phase)(f: => T): T =
      global.enteringPhase(phase)(f)
    def afterPhase[T](phase: Phase)(f: => T): T =
      global.exitingPhase(phase)(f)
  }
}

trait ScalacVersion {
  lazy val scalaBinaryVersion = "2.11"
  lazy val scalaVersion = "2.11.1"
}

