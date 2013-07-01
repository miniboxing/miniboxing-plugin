package miniboxing.plugin

import scala.tools.nsc
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.transform.InfoTransform
import scala.tools.nsc.transform.TypingTransformers

trait MiniboxComponent extends
    PluginComponent
    with MiniboxLogic
    with MiniboxInfoTransformation
    with MiniboxLogging
    with MiniboxTreeTransformation
    with MiniboxTreeSpecializer
    with MiniboxPeepholeTransformation
    with MiniboxSpecializationInfo
    with MiniboxDefinitions
    with MiniboxPhase

class Minibox(val global: Global) extends Plugin {
  import global._

  val name = "minibox"
  val description = "spcializes generic classes"
  val components = List[PluginComponent](Component)

  var flag_log = sys.props.get("miniboxing.log").isDefined
  var flag_debug = sys.props.get("miniboxing.debug").isDefined
  var flag_stats = sys.props.get("miniboxing.stats").isDefined

  override def processOptions(options: List[String], error: String => Unit) {
    for (option <- options) {
      if (option.toLowerCase() == "log")
        flag_log = true
      else if (option.toLowerCase() == "debug")
        flag_debug = true
      else if (option.toLowerCase() == "stats")
        flag_stats = true
      else
        error("Miniboxing: Option not understood: " + option)
    }
  }

  override val optionsHelp: Option[String] = Some(
    s"  -P:${name}:log               log miniboxing signature transformations\n" +
    s"  -P:${name}:stats             log miniboxing tree transformations (verbose logging)\n" +
    s"  -P:${name}:debug             debug logging for the miniboxing plugin (rarely used)")

  private object Component extends MiniboxComponent {

    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = List("refchecks")
    override val runsRightAfter = Some("uncurry")
    val phaseName = Minibox.this.name

    def flag_log = Minibox.this.flag_log
    def flag_debug = Minibox.this.flag_debug
    def flag_stats = Minibox.this.flag_stats

    override var currentPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      currentPhase = new Phase(prev);
      currentPhase
    }

    override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
      override def transform(tree: Tree) = {
        // execute the tree transformer after all symbols have been processed
        val tree1 = afterMinibox(new MiniboxTreeTransformer(unit).transform(tree))
        val tree2 = afterMinibox(new MiniboxPeepholeTransformer(unit).transform(tree1))
        tree2.foreach(tree => assert(tree.tpe != null, tree))
        tree2
      }
    }
  }
}

trait MiniboxPhase extends PluginComponent {
  var currentPhase : StdPhase

  def afterMinibox[T](op: => T): T =
    global.afterPhase(currentPhase)(op)

  def beforeMinibox[T](op: => T): T =
    global.beforePhase(currentPhase)(op)
}
