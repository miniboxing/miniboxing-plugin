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

  private object Component extends PluginComponent with  
    MiniboxLogic with MiniboxInfoTransformation with MiniboxLogging  with MiniboxTreeTransformation {
    
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = List("refchecks");
    val phaseName = Minibox.this.name

    override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
      override def transform(tree: Tree) = {
        // execute the tree transformer after all symbols have been processed
        //XXX: implicit resolution does not work with afterSpecialize(...)
            new MiniboxTreeTransformer(unit).transform(tree)
      }
    }
  }
}


