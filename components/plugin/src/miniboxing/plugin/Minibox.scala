//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
package miniboxing.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform._
import metadata._
import transform._
import interop.inject._
import interop.bridge._
import interop.coerce._
import interop.commit._
import minibox.inject._
import minibox.bridge._
import minibox.coerce._
import minibox.commit._
import hijack._
import prepare._
import infrastructure._
import tweakerasure._
import scala.tools.nsc.settings.ScalaVersion


/** Main miniboxing class */
class Minibox(val global: Global) extends Plugin with ScalacVersion {
  import global._

  val name = "minibox"
  val description = "Specializes generic classes"

  VersionChecker.versionMessage() match {
    case Some(msg) =>
      global.warning(msg)
    case None =>
  }

  lazy val components = {
    // and here are the compiler phases miniboxing introduces:
    List[PluginComponent](PreTyperPhase,
                          PostTyperPhase,
                          InteropInjectPhase,
                          PreparePhase,
                          InteropBridgePhase,
                          InteropCoercePhase,
                          InteropCommitPhase,
                          HijackPhase,
                          MiniboxInjectPhase,
                          MiniboxBridgePhase,
                          MiniboxCoercePhase,
                          MiniboxCommitPhase,
                          TweakErasurePhase,
                          CompileTimeOnlyAddTagsPhase,
                          CompileTimeOnlyRemoveTagsPhase)
  }

  // LDL Coercions
  global.addAnnotationChecker(MiniboxCoercePhase.StorageAnnotationChecker)
  global.addAnnotationChecker(InteropCoercePhase.mbFunctionAnnotationChecker)

  trait MiniboxingCommon extends CommonDefinitions {

    val global: Global

    var flag_log = sys.props.get("miniboxing.log").isDefined
    var flag_debug = sys.props.get("miniboxing.debug").isDefined
    var flag_stats = sys.props.get("miniboxing.stats").isDefined
    var flag_hijack_spec = sys.props.get("miniboxing.hijack.spec").isDefined
    var flag_spec_no_opt = sys.props.get("miniboxing.Commit.no-opt").isDefined
    var flag_loader_friendly = sys.props.get("miniboxing.loader").isDefined
    var flag_two_way = true
    var flag_rewire_functionX_values = true
    var flag_rewire_functionX_repres = true
    var flag_rewire_functionX_bridges = true
    var flag_mark_all = false // type parameters as @miniboxed
    var flag_strict_typechecking = false
    var flag_strip_miniboxed = false
    var flag_create_local_specs = true
    var flag_strict_warnings = true
    var flag_strict_warnings_outside = false
    var flag_warn_mbarrays = true
    var flag_rewire_functionX_application = true
    var flag_rewire_mbarray = true
    var flag_rewire_tuples = true
    var flag_constructor_spec = true
  }

  lazy val common = new MiniboxingCommon {
    val global: Minibox.this.global.type = Minibox.this.global
  }

  override def processOptions(options: List[String], error: String => Unit) {

    import common._

    for (option <- options) {
      option.toLowerCase() match {

        // Basic (and documented) miniboxing compiler flags:

        case "log" =>
          flag_log = true
        case "debug" =>
          flag_debug = true
        case "stats" =>
          flag_stats = true
        case "hijack" =>
          flag_hijack_spec = true
        case "spec-no-opt" =>
          flag_spec_no_opt = true
        case "loader" =>
          flag_loader_friendly = true
        case "warn" =>
          this.global.reporter.echo("Miniboxing plugin warning: Showing performance warnings became the default behavior " +
                                    "of the miniboxing plugin. To hide warnings, please use the -P:minibox:warn-off " +
                                    "Scala compiler flag. On the other hand, if you want cross-library warnings, " +
                                    "please use the -P:minibox:warn-all flag. Read more about the miniboxing warnings at " +
                                    "http://scala-miniboxing.org/2014/10/21/miniboxing-warnings.html.")
        case "warn-off" =>
          flag_strict_warnings = false
          flag_warn_mbarrays = false
        case "warn-all" =>
          flag_strict_warnings = true
          flag_strict_warnings_outside = true
        case "warn-mbarrays-off" =>
          flag_warn_mbarrays = false
        case "mark-all" =>
          flag_mark_all = true

        // The following flags are undocumented, since they control options that transform the miniboxing compilation
        // scheme in (possibly) binary incompatible ways, thus are not explosed by default to the user. Should

        case "yone-way" =>                       // Undocumented flag, only used for running the test suite,
          flag_two_way = false                   // where the tests required the one-way translation
        case "two-way" =>
          this.global.reporter.echo("Miniboxing plugin warning: The two-way transformation (with long and double as " +
                                    "storage types) has become default in version 0.4 version of the miniboxing plugin, " +
                                    "so there is no need to specify it in the command line")
        case "ygen-brdgs" =>                     // Undocumented flag, only used for running the test suite
          flag_rewire_functionX_bridges = false  // while avoiding func. to miniboxed func. bridge optimization
        case "ystrip-miniboxed" =>
          flag_strip_miniboxed = true
        case "yno-local-specs" =>
          flag_create_local_specs = false
        case "ykeep-functionx-values" | "library-functions"=>
          flag_rewire_functionX_values = false
          flag_rewire_functionX_repres = false
          flag_rewire_functionX_application = false
          flag_rewire_functionX_bridges = false

        // The following flags are undocumented, since they control internal miniboxing plugin features, which
        // should not be used directly by the programmers (they are mainly here to allow reproducing test cases)

        case "ykeep-functionx-repres" =>
          flag_rewire_functionX_repres = false
        case "ystrict-typechecking" =>
          flag_strict_typechecking = true
        case "ykeep-mbarray-generic" =>
          flag_rewire_mbarray = false
        case "ykeep-tuples-generic" =>
          flag_rewire_tuples = false
        case "yrewire-functionx-application" =>
          this.global.reporter.echo("Miniboxing plugin warning: The function application specialization is now the default " +
                                   s"miniboxing plugin behavior, so there is no need to use the -P:minibox:$option flag " +
                                    "anymore. To leave function applications generic, please use the " +
                                    "-P:minibox:Ykeep-functionX-application flag.")
          flag_rewire_functionX_application = true
        case "ykeep-functionx-application" =>
          flag_rewire_functionX_application = false
        case "ygeneric-constructor-code" =>
          flag_constructor_spec = false
        case "off" =>
          this.global.reporter.echo("Miniboxing plugin warning: Turning off all minboxing specialization!")
          flag_rewire_functionX_values = false
          flag_rewire_functionX_repres = false
          flag_rewire_functionX_bridges = false
          flag_rewire_functionX_application = false
          flag_strip_miniboxed = true
          flag_create_local_specs = false
          flag_strict_warnings = false
          flag_strict_warnings_outside = false
          flag_warn_mbarrays = false
          flag_rewire_mbarray = false
          flag_rewire_tuples = false
          flag_constructor_spec = false
        case _ =>
          error("Miniboxing: Option not understood: " + option)
      }
    }
  }

  override val optionsHelp: Option[String] = Some(Seq(
    s"  -P:${name}:warn-off          do not show performance and specialization warnings for your code",
    s"  -P:${name}:warn-all          show cross-project warnings, aka warn for the libraries as well",
    s"  -P:${name}:warn-mbarrys-off  do not show warnings suggesting use of MbArray instead of Array",
    s"  -P:${name}:hijack            hijack the @specialized(...) notation for miniboxing",
    s"  -P:${name}:mark-all          implicitly add @miniboxed annotations to all type parameters",
    s"  -P:${name}:log               log miniboxing signature transformations").mkString("\n"))


  // main phases:
  def MiniboxInjectPhaseName = "minibox-inject"
  def MiniboxBridgePhaseName = "minibox-bridge"
  def MiniboxCoercePhaseName = "minibox-coerce"
  def MiniboxCommitPhaseName = "minibox-commit"
  def InteropInjectPhaseName = "interop-inject"
  def InteropBridgePhaseName = "interop-bridge"
  def InteropCoercePhaseName = "interop-coerce"
  def InteropCommitPhaseName = "interop-commit"

  // helper phases:
  def PreparePhaseName = "mb-ext-prepare"
  def PostTyperPhaseName = "mb-ext-post-tpe"
  def PreTyperPhaseName = "mb-ext-pre-tpe"
  def HijackPhaseName = "mb-ext-hijacker"
  def TweakErasurePhaseName = "mb-tweak-erasure"
  def CompileTimeOnlyAddTagsPhaseName = "mb-compile-time-only-add-tags"
  def CompileTimeOnlyRemoveTagsPhaseName = "mb-compile-time-only-remove-tags"

  // outside phases:
  def TyperPhaseName = "typer"
  def PatternMatcherPhaseName = "patmat"
  def ExtensionMethodsPhaseName = "extmethods"
  def ParserPhaseName = "parser"
  def UncurryPhaseName = "uncurry"
  def PostErasurePhaseName = "posterasure"
  def PicklerPhaseName = "pickler"

  private object HijackPhase extends {
    val common = Minibox.this.common
  } with HijackComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(ExtensionMethodsPhaseName)
    val phaseName = HijackPhaseName

    // no change
    override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
      override def transform(tree: Tree) = tree
    }
  }

  private object InteropInjectPhase extends {
    val common = Minibox.this.common
  } with InteropInjectComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(PatternMatcherPhaseName)
    val phaseName = InteropInjectPhaseName

    var interopInjectPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      interopInjectPhase = new Phase(prev)
      interopInjectPhase
    }
  }

  private object PreparePhase extends {
    val interop: InteropInjectPhase.type = InteropInjectPhase
    val common = Minibox.this.common
  } with PrepareComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(UncurryPhaseName)
    val phaseName = PreparePhaseName

    var preparePhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      preparePhase = new PreparePhaseImpl(prev)
      preparePhase
    }
  }

  private object InteropBridgePhase extends {
    val interop: InteropInjectPhase.type = InteropInjectPhase
    val common = Minibox.this.common
  } with InteropBridgeComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(PreparePhaseName)
    val phaseName = InteropBridgePhaseName

    var interopBridgePhase : StdPhase = _
    def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      interopBridgePhase = new BridgePhase(prev.asInstanceOf[StdPhase])
      interopBridgePhase
    }
  }

  private object InteropCoercePhase extends {
    val interop: InteropInjectPhase.type = InteropInjectPhase
    val common = Minibox.this.common
  } with InteropCoerceComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(InteropBridgePhaseName)
    val phaseName = InteropCoercePhaseName

    var interopCoercePhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      interopCoercePhase = new CoercePhase(prev)
      interopCoercePhase
    }
  }

  private object InteropCommitPhase extends {
    val interop: InteropInjectPhase.type = InteropInjectPhase
    val common = Minibox.this.common
  } with InteropCommitComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(InteropCoercePhaseName)
    val phaseName = InteropCommitPhaseName

    def minibox = MiniboxInjectPhase

    var interopCommitPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      interopCommitPhase = new Phase(prev)
      interopCommitPhase
    }
  }

  private object MiniboxInjectPhase extends {
    val common = Minibox.this.common
  } with MiniboxInjectComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(PostTyperPhaseName)
    val phaseName = MiniboxInjectPhaseName

    var mboxInjectPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      mboxInjectPhase = new Phase(prev)
      mboxInjectPhase
    }

    override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
      override def transform(tree: Tree) = {
        // execute the tree transformer after all symbols have been processed
        val tree1 = afterMiniboxInject(new MiniboxTreeTransformer(unit).transform(tree))
        //tree1.foreach(tree => assert(tree.tpe != null, "tree not typed: " + tree))
        tree1
      }
    }
  }

  private object MiniboxBridgePhase extends {
    val minibox: MiniboxInjectPhase.type = MiniboxInjectPhase
    val common = Minibox.this.common
  } with MiniboxBridgeComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(MiniboxInjectPhaseName)
    val phaseName = MiniboxBridgePhaseName

    var mboxBridgePhase : StdPhase = _
    def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      mboxBridgePhase = new BridgePhase(prev.asInstanceOf[minibox.Phase])
      mboxBridgePhase
    }
  }

  private object MiniboxCoercePhase extends {
    val minibox: MiniboxInjectPhase.type = MiniboxInjectPhase
    val common = Minibox.this.common
  } with MiniboxCoerceComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(MiniboxBridgePhaseName)
    val phaseName = MiniboxCoercePhaseName

    var mboxCoercePhase : StdPhase = _
    def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      mboxCoercePhase = new CoercePhase(prev.asInstanceOf[StdPhase])
      mboxCoercePhase
    }
  }

  private object MiniboxCommitPhase extends {
    val minibox: MiniboxInjectPhase.type = MiniboxInjectPhase
    val interop: InteropInjectPhase.type = InteropInjectPhase
    val common = Minibox.this.common
  } with MiniboxCommitComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(MiniboxCoercePhaseName)
    val phaseName = MiniboxCommitPhaseName

    var mboxCommitPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      mboxCommitPhase = new Phase(prev)
      mboxCommitPhase
    }
  }

  private object PreTyperPhase extends {
    val minibox: MiniboxInjectPhase.type = MiniboxInjectPhase
    val common = Minibox.this.common
  } with PreTyperComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(ParserPhaseName)
    val phaseName = PreTyperPhaseName

    def newPhase(_prev: Phase) = new StdPhase(_prev) {
      override def name = PreTyperPhaseName
      def apply(unit: CompilationUnit) {
        import global._
        import global.Flag._

        minibox.preMiniboxingFlags()
      }
    }
  }

  private object PostTyperPhase extends {
    val minibox: MiniboxInjectPhase.type = MiniboxInjectPhase
    val common = Minibox.this.common
  } with PreTyperComponent
    with ScalacVersion {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(InteropCommitPhaseName)
    val phaseName = PostTyperPhaseName

    def newPhase(_prev: Phase) = new StdPhase(_prev) {
      override def name = PostTyperPhaseName
      def apply(unit: CompilationUnit) {
        import global._
        import global.Flag._

        minibox.postMiniboxingFlags()
      }
    }
  }

  private object TweakErasurePhase extends {
    val interop: InteropInjectPhase.type = InteropInjectPhase
    val common = Minibox.this.common
  } with TweakErasureComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(PostErasurePhaseName)
    val phaseName = TweakErasurePhaseName

    var tweakErasurePhase: Phase = _

    def newPhase(_prev: Phase) = {
      tweakErasurePhase = new TweakErasurePhase(_prev)
      tweakErasurePhase
    }
  }

  private object CompileTimeOnlyAddTagsPhase extends {
    val common = Minibox.this.common
  } with CompileTimeOnlyAddTagsComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(HijackPhaseName)
    val phaseName = CompileTimeOnlyAddTagsPhaseName

    var addCompileOnlyPhase: Phase = _

    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      addCompileOnlyPhase = new Phase(prev)
      addCompileOnlyPhase
    }

    override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
      override def transform(tree: Tree) = {
        if (tree.hasSymbolField)
          afterAddCompileOnly(tree.symbol.info)
        super.transform(tree)
      }
    }
  }

  private object CompileTimeOnlyRemoveTagsPhase extends {
    val common = Minibox.this.common
  } with CompileTimeOnlyRemoveTagsComponent {
    val global: Minibox.this.global.type = Minibox.this.global
    val runsAfter = Nil
    override val runsRightAfter = Some(PicklerPhaseName)
    val phaseName = CompileTimeOnlyRemoveTagsPhaseName


    var removeCompileOnlyPhase: Phase = _

    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      removeCompileOnlyPhase = new Phase(prev)
      removeCompileOnlyPhase
    }

    override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
      override def transform(tree: Tree) = {
        if (tree.hasSymbolField)
          afterRemoveCompileOnly(tree.symbol.info)
        super.transform(tree)
      }
    }
  }
}
