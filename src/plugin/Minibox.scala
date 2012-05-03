package plugin

import scala.tools.nsc
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.transform.InfoTransform
import scala.tools.nsc.transform.TypingTransformers

class Minibox(val global: Global) extends Plugin {
  import global._

  val name = "minibox"
  val description = "checks for division by zero"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with InfoTransform with TypingTransformers with 
    MiniboxLogic with MiniboxTransformation with MiniboxLogging {
    
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = List("refchecks");
    val phaseName = Minibox.this.name

    // just patching through to the MiniboxTransformation methods
    override def transformInfo(sym: Symbol, tpe: Type): Type = miniboxTransformInfo(sym, tpe)
    override def newTransformer(unit: CompilationUnit): Transformer = new MiniboxTransformer(unit)
  }
}


