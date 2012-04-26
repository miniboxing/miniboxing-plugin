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

  private object Component extends PluginComponent with InfoTransform with TypingTransformers {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = List("refchecks");
    val phaseName = Minibox.this.name
    //def newPhase(_prev: Phase): StdPhase =
    import scala.tools.nsc.Phase
    //def newPhase(prev: Phase): StdPhase = new MiniboxPhase(prev)

    def transformInfo(sym: Symbol,tpe: Type): Type = {println("type: " + tpe); tpe}
    def newTransformer(unit: CompilationUnit): Transformer = new Transformer() {
      override def transform(tree) = {println("tree: " + tree); tree}
    }

//    val x = 1
//    class MiniboxPhase(prev: Phase) extends StdPhase(prev) {
//      override def name = Minibox.this.name
//      def apply(unit: CompilationUnit) {
//        for ( tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body;
//             if rcvr.tpe <:< definitions.IntClass.tpe) 
//          {
//            unit.error(tree.pos, "definitely division by zero")
//          }
//      }
//    }
  }
}


