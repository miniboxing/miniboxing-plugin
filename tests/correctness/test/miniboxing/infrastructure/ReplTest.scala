/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

/* This part of the code is taken from
 * https://github.com/scala/scala/blob/2.11.x/src/partest-extras/scala/tools/partest/ReplTest.scala
 * which didn't make it into the distribution jar
 */

package miniboxing.infrastructure

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop
import java.lang.reflect.{ Method => JMethod, Field => JField }
import scala.tools.partest.DirectTest

class ReplTest(val code: String, val flags: String) extends DirectTest {

  override def extraSettings: String = if (flags eq null) "" else flags

  final override def settings: Settings = {
    val s = super.settings
    // s.Yreplsync.value = true
    s.Xnojline.value = true
    s
  }

  def show() = ???

  def replOutput(): String = {
    val s = settings
    val lines = ILoop.runForTranscript(code, s).lines
    lines.filter(!_.startsWith("Welcome to Scala")).mkString("\n")
  }
}