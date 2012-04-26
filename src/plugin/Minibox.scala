package plugin

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class Minibox(val global: Global) extends Plugin {
  import global._

  val name = "minibox"
  val description = "checks for division by zero"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = List("refchecks");
    val phaseName = Minibox.this.name
    def newPhase(_prev: Phase) = new MiniboxPhase(_prev)    

    val x = 1
    class MiniboxPhase(prev: Phase) extends StdPhase(prev) {
      override def name = Minibox.this.name
      def apply(unit: CompilationUnit) {
        for ( tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body;
             if rcvr.tpe <:< definitions.IntClass.tpe) 
          {
            unit.error(tree.pos, "definitely division by zero")
          }
      }
    }
  }
}


