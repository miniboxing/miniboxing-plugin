//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2012-2014 Scala Team, École polytechnique fédérale de Lausanne
//
package miniboxing.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform._
import metadata._
import transform._
import inject._
import coerce._
import commit._
import hijack._

/** Specialization hijacking component `@specialized T -> @miniboxed T` */
trait HijackComponent extends
    PluginComponent
    with MiniboxInfoHijack
    with MiniboxDefinitions
    with ScalacCrossCompilingLayer {

  def flag_hijack_spec: Boolean
}

/** Injecticator component `def t -> def t_L, def t_J` */
trait MiniboxInjectComponent extends
    PluginComponent
    with MiniboxLogging
    with MiniboxDefinitions
    with MiniboxNameUtils
    with MiniboxMetadata
    with MiniboxMetadataUtils
    with MiniboxMetadataAddons
    with MiniboxMethodInfo
    with MiniboxInjectInfoTransformation
    with MiniboxInjectTreeTransformation
    with TreeRewriters
    with ScalacCrossCompilingLayer {

  def mboxInjectPhase: StdPhase

  def afterMiniboxInject[T](op: => T): T = global.afterPhase(mboxInjectPhase)(op)
  def beforeMiniboxInject[T](op: => T): T = global.beforePhase(mboxInjectPhase)(op)

  def flag_log: Boolean
  def flag_debug: Boolean
  def flag_stats: Boolean
  def flag_spec_no_opt: Boolean
  def flag_loader_friendly: Boolean
  def flag_two_way: Boolean
}

/** Introduces explicit Coerceations from `T` to `@storage T` and back */
trait MiniboxCoerceComponent extends
    PluginComponent
    with MiniboxCoerceTreeTransformer
    with MiniboxAnnotationCheckers
    with ScalacCrossCompilingLayer {

  val minibox: MiniboxInjectComponent { val global: MiniboxCoerceComponent.this.global.type }

  def mboxCoercePhase: StdPhase

  def afterMiniboxCoerce[T](op: => T): T = global.afterPhase(mboxCoercePhase)(op)
  def beforeMiniboxCoerce[T](op: => T): T = global.beforePhase(mboxCoercePhase)(op)
}


/** Specializer component `T @storage -> Long` */
trait MiniboxCommitComponent extends
    PluginComponent
    with MiniboxCommitInfoTransformer
    with MiniboxCommitTreeTransformer
    with ScalacCrossCompilingLayer {

  val minibox: MiniboxInjectComponent { val global: MiniboxCommitComponent.this.global.type }

  def mboxCommitPhase: StdPhase

  def afterMiniboxCommit[T](op: => T): T = global.afterPhase(mboxCommitPhase)(op)
  def beforeMiniboxCommit[T](op: => T): T = global.beforePhase(mboxCommitPhase)(op)

  def flag_log: Boolean
  def flag_debug: Boolean
  def flag_stats: Boolean
  def flag_two_way: Boolean
}

trait PreTyperComponent extends
  PluginComponent
  with TypingTransformers
  with ScalacCrossCompilingLayer {

  val miniboxing: MiniboxInjectComponent { val global: PreTyperComponent.this.global.type }
}

trait PostTyperComponent extends
  PluginComponent
  with TypingTransformers
  with ScalacCrossCompilingLayer {

  import global._
  import global.Flag._
  val miniboxing: MiniboxInjectComponent { val global: PostTyperComponent.this.global.type }
}

/** Main miniboxing class */
class Minibox(val global: Global) extends Plugin {
  import global._

  val name = "minibox"
  val description = "Specializes generic classes"

  lazy val components = {
    if (!flag_no_logo) {

      def printLogo() =
        Console.println("""
          |     _____   .__         .__ ____.                     .__ scala-miniboxing.org
          |    /     \  |__|  ____  |__|\_  |__    _____  ___  ___|__|  ____    _____
          |   /  \ /  \ |  | /    \ |  | |  __ \  /  ___\ \  \/  /|  | /    \  /  ___\
          |  /    Y    \|  ||   |  \|  | |  \_\ \(  (_)  ) >    < |  ||   |  \(  /_/  )
          |  \____|__  /|__||___|  /|__| |____  / \_____/ /__/\_ \|__||___|  / \___  /
          |          \/          \/           \/                \/         \/ /_____/
          | Copyright (c) 2012-2014 Scala Team, École polytechnique fédérale de Lausanne.""".stripMargin)

      // printLogo()
    }

    // and here are the compiler phases miniboxing introduces:
    List[PluginComponent](PreTyperPhase,
                          PostTyperPhase,
                          HijackPhase,
                          MiniboxInjectPhase,
                          MiniboxCoercePhase,
                          MiniboxCommitPhase)
  }

  // LDL Coerceation
  global.addAnnotationChecker(MiniboxCoercePhase.StorageAnnotationChecker)

  var flag_log = sys.props.get("miniboxing.log").isDefined
  var flag_debug = sys.props.get("miniboxing.debug").isDefined
  var flag_stats = sys.props.get("miniboxing.stats").isDefined
  var flag_hijack_spec = sys.props.get("miniboxing.hijack.spec").isDefined
  var flag_spec_no_opt = sys.props.get("miniboxing.Commit.no-opt").isDefined
  var flag_loader_friendly = sys.props.get("miniboxing.loader").isDefined
  var flag_no_logo = sys.props.get("miniboxing.no-logo").isDefined
  var flag_two_way = sys.props.get("miniboxing.two-way").isDefined

  override def processOptions(options: List[String], error: String => Unit) {
    for (option <- options) {
      if (option.toLowerCase() == "log")
        flag_log = true
      else if (option.toLowerCase() == "debug")
        flag_debug = true
      else if (option.toLowerCase() == "stats")
        flag_stats = true
      else if (option.toLowerCase() == "hijack")
        flag_hijack_spec = true
      else if (option.toLowerCase() == "spec-no-opt")
        flag_spec_no_opt = true
      else if (option.toLowerCase() == "loader")
        flag_loader_friendly = true
      else if (option.toLowerCase() == "no-logo")
        flag_no_logo = true
      else if (option.toLowerCase() == "two-way")
        flag_two_way = true
      else
        error("Miniboxing: Option not understood: " + option)
    }
  }

  override val optionsHelp: Option[String] = Some(
    s"  -P:${name}:log               log miniboxing signature transformations\n" +
    s"  -P:${name}:stats             log miniboxing tree transformations (verbose logging)\n" +
    s"  -P:${name}:debug             debug logging for the miniboxing plugin (rarely used)\n" +
    s"  -P:${name}:hijack            hijack the @specialized(...) notation for miniboxing\n" +
    s"  -P:${name}:spec-no-opt       don't optimize method specialization, do create useless specializations\n" +
    s"  -P:${name}:loader            generate classloader-friendly code (but more verbose)\n" +
    s"  -P:${name}:no-logo           skip the miniboxing logo display\n" +
    s"  -P:${name}:two-way           generate variants for long and double instead of just double")

  private object HijackPhase extends HijackComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = List("typer")
    override val runsRightAfter = Some("extmethods")
    val phaseName = "hijacker"

    def flag_hijack_spec = Minibox.this.flag_hijack_spec
    def flag_two_way = Minibox.this.flag_two_way

    // no change
    override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
      override def transform(tree: Tree) = tree
    }
  }

  private object MiniboxInjectPhase extends MiniboxInjectComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = List("refchecks")
    override val runsRightAfter = Some("uncurry")
    val phaseName = Minibox.this.name + "-inject"

    def flag_log = Minibox.this.flag_log
    def flag_debug = Minibox.this.flag_debug
    def flag_stats = Minibox.this.flag_stats
    def flag_spec_no_opt = Minibox.this.flag_spec_no_opt
    def flag_loader_friendly = Minibox.this.flag_loader_friendly
    def flag_two_way = Minibox.this.flag_two_way

    var mboxInjectPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      mboxInjectPhase = new Phase(prev)
      mboxInjectPhase
    }

    override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
      override def transform(tree: Tree) = {
        // execute the tree transformer after all symbols have been processed
        val tree1 = afterMiniboxInject(new MiniboxTreeTransformer(unit).transform(tree))
        tree1.foreach(tree => assert(tree.tpe != null, "tree not typed: " + tree))
        tree1
      }
    }
  }

  private object MiniboxCoercePhase extends {
    val minibox: MiniboxInjectPhase.type = MiniboxInjectPhase
  } with MiniboxCoerceComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = List(MiniboxInjectPhase.phaseName)
    override val runsRightAfter = Some(MiniboxInjectPhase.phaseName)
    val phaseName = Minibox.this.name + "-coerce"

    var mboxCoercePhase : StdPhase = _
    def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      mboxCoercePhase = new CoercePhase(prev.asInstanceOf[minibox.Phase])
      mboxCoercePhase
    }
  }

  private object MiniboxCommitPhase extends {
    val minibox: MiniboxInjectPhase.type = MiniboxInjectPhase
  } with MiniboxCommitComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = List(MiniboxCoercePhase.phaseName)
    override val runsRightAfter = Some(MiniboxCoercePhase.phaseName)
    val phaseName = Minibox.this.name + "-commit"

    def flag_log = Minibox.this.flag_log
    def flag_debug = Minibox.this.flag_debug
    def flag_stats = Minibox.this.flag_stats
    def flag_two_way = Minibox.this.flag_two_way

    var mboxCommitPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      mboxCommitPhase = new Phase(prev)
      mboxCommitPhase
    }
  }

  private object PreTyperPhase extends {
    val miniboxing: MiniboxInjectPhase.type = MiniboxInjectPhase
  } with PreTyperComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = List()
    override val runsRightAfter = Some("parser")
    val phaseName = "mb-pretyper"

    def newPhase(_prev: Phase) = new StdPhase(_prev) {
      override def name = PreTyperPhase.phaseName
      def apply(unit: CompilationUnit) {
        import global._
        import global.Flag._
        for (sym <- miniboxing.metadata.allStemClasses)
          if (miniboxing.metadata.classStemTraitFlag(sym))
            sym.resetFlag(ABSTRACT)
          else
            sym.resetFlag(ABSTRACT | TRAIT)
      }
    }
  }

  private object PostTyperPhase extends {
    val miniboxing: MiniboxInjectPhase.type = MiniboxInjectPhase
  } with PreTyperComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = List("typer")
    //override val runsRightAfter = Some("typer")
    val phaseName = "mb-posttyper"

    def newPhase(_prev: Phase) = new StdPhase(_prev) {
      override def name = PostTyperPhase.phaseName
      def apply(unit: CompilationUnit) {
        import global._
        import global.Flag._
        for (sym <- miniboxing.metadata.allStemClasses)
          sym.setFlag(ABSTRACT | TRAIT)
      }
    }
  }
}
