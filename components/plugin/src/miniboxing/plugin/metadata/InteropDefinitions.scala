//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Vlad Ureche
//
package miniboxing.plugin
package metadata

import scala.tools.nsc.plugins.PluginComponent
import scala.collection.immutable.ListMap

trait InteropDefinitions {
  this: PluginComponent =>

  import global._
  import definitions._

  lazy val mbFunctionClass = global.rootMirror.getRequiredClass("miniboxing.mbFunction")
  lazy val apiClass = global.rootMirror.getRequiredClass("scala.api")

  lazy val Function0Class = global.definitions.FunctionClass(0)
  lazy val Function1Class = global.definitions.FunctionClass(1)
  lazy val Function2Class = global.definitions.FunctionClass(2)
  lazy val AbstractFunction0Class = global.definitions.AbstractFunctionClass(0)
  lazy val AbstractFunction1Class = global.definitions.AbstractFunctionClass(1)
  lazy val AbstractFunction2Class = global.definitions.AbstractFunctionClass(2)
  lazy val FunctionSyms = List(Function0Class, Function1Class, Function2Class)
  lazy val AbstractFunctions = List(AbstractFunction0Class, AbstractFunction1Class, AbstractFunction2Class)

  lazy val MiniboxedFunction0Class = global.rootMirror.getRequiredClass("miniboxing.runtime.MiniboxedFunction0")
  lazy val MiniboxedFunction1Class = global.rootMirror.getRequiredClass("miniboxing.runtime.MiniboxedFunction1")
  lazy val MiniboxedFunction2Class = global.rootMirror.getRequiredClass("miniboxing.runtime.MiniboxedFunction2")
  lazy val MiniboxedFunction0PolyTpe = PolyType(MiniboxedFunction0Class.typeParams, MiniboxedFunction0Class.tpe)
  lazy val MiniboxedFunction1PolyTpe = PolyType(MiniboxedFunction1Class.typeParams, MiniboxedFunction1Class.tpe)
  lazy val MiniboxedFunction2PolyTpe = PolyType(MiniboxedFunction2Class.typeParams, MiniboxedFunction2Class.tpe)
  lazy val AbstractMiniboxedFunction0Class = global.rootMirror.getRequiredClass("miniboxing.runtime.AbstractMiniboxedFunction0")
  lazy val AbstractMiniboxedFunction1Class = global.rootMirror.getRequiredClass("miniboxing.runtime.AbstractMiniboxedFunction1")
  lazy val AbstractMiniboxedFunction2Class = global.rootMirror.getRequiredClass("miniboxing.runtime.AbstractMiniboxedFunction2")
  lazy val AbstractMiniboxedFunction0PolyTpe = PolyType(AbstractMiniboxedFunction0Class.typeParams, AbstractMiniboxedFunction0Class.tpe)
  lazy val AbstractMiniboxedFunction1PolyTpe = PolyType(AbstractMiniboxedFunction1Class.typeParams, AbstractMiniboxedFunction1Class.tpe)
  lazy val AbstractMiniboxedFunction2PolyTpe = PolyType(AbstractMiniboxedFunction2Class.typeParams, AbstractMiniboxedFunction2Class.tpe)

  lazy val abstractFunctionToAbstractMiniboxedTpe: Map[Symbol, Type] =
    Map(AbstractFunction0Class -> AbstractMiniboxedFunction0PolyTpe,
        AbstractFunction1Class -> AbstractMiniboxedFunction1PolyTpe,
        AbstractFunction2Class -> AbstractMiniboxedFunction2PolyTpe)

  lazy val FunctionsObjectSymbol = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxedFunctionBridge")

  //   def marker_mbfun2fun[T](t: T @mbFunction): T
  lazy val marker_mbfun2fun =
    newPolyMethod(1, FunctionsObjectSymbol, newTermName("marker_mbfun2fun"), 0L)(
      tpar => (Some(List(tpar(0).tpeHK.withMbFunction)), tpar(0).tpeHK))
  //   def marker_fun2mbfun[T](t: T): T @storage
  lazy val marker_fun2mbfun =
    newPolyMethod(1, FunctionsObjectSymbol, newTermName("marker_fun2mbfun"), 0L)(
      tpar => (Some(List(tpar(0).tpeHK)), tpar(0).tpeHK.withMbFunction))

  def bridgeSymbol(name: String) = definitions.getMember(FunctionsObjectSymbol, newTermName(name))

  lazy val function0_bridge = bridgeSymbol("function0_bridge")
  lazy val function1_bridge = bridgeSymbol("function1_bridge")
  lazy val function2_bridge = bridgeSymbol("function2_bridge")

  lazy val function_bridges = Set(function0_bridge, function1_bridge, function2_bridge)
  lazy val function_bridge_optimized: Map[Symbol, Map[List[Symbol], Symbol]] =
    Map(
      function0_bridge ->
        Map(
          List(LongClass)   -> bridgeSymbol("function0_opt_bridge_long"),
          List(DoubleClass) -> bridgeSymbol("function0_opt_bridge_double")
        ),
      function1_bridge ->
        Map(
          List(LongClass, LongClass)     -> bridgeSymbol("function1_opt_bridge_long_long"),
          List(LongClass, DoubleClass)   -> bridgeSymbol("function1_opt_bridge_long_double"),
          List(DoubleClass, LongClass)   -> bridgeSymbol("function1_opt_bridge_double_long"),
          List(DoubleClass, DoubleClass) -> bridgeSymbol("function1_opt_bridge_double_double")
        ),
      function2_bridge ->
        Map(
          List(LongClass, LongClass, LongClass)       -> bridgeSymbol("function2_opt_bridge_long_long_long"),
          List(LongClass, LongClass, DoubleClass)     -> bridgeSymbol("function2_opt_bridge_long_long_double"),
          List(LongClass, DoubleClass, LongClass)     -> bridgeSymbol("function2_opt_bridge_long_double_long"),
          List(LongClass, DoubleClass, DoubleClass)   -> bridgeSymbol("function2_opt_bridge_long_double_double"),
          List(DoubleClass, LongClass, LongClass)     -> bridgeSymbol("function2_opt_bridge_double_long_long"),
          List(DoubleClass, LongClass, DoubleClass)   -> bridgeSymbol("function2_opt_bridge_double_long_double"),
          List(DoubleClass, DoubleClass, LongClass)   -> bridgeSymbol("function2_opt_bridge_double_double_long"),
          List(DoubleClass, DoubleClass, DoubleClass) -> bridgeSymbol("function2_opt_bridge_double_double_double")
        )
    )

  def flag_rewire_functionX_values: Boolean
  def flag_rewire_functionX_repres: Boolean
  def flag_rewire_functionX_bridges: Boolean

  lazy val libraryFunctionName = newTermName("extractFunctionX")

  // Addons, not yet separated:

  implicit class RichType(tpe: Type) {
    def hasApiAnnotation: Boolean = tpe.hasAnnotation(apiClass)
    def withoutApiAnnotations: Type = tpe.filterAnnotations(_.tpe.typeSymbol != apiClass)

    def isMbFunction: Boolean = tpe.dealiasWiden.annotations.exists(_.tpe.typeSymbol == mbFunctionClass)
    def withMbFunction: Type = tpe.withAnnotations(List(Annotation.apply(mbFunctionClass.tpe, Nil, ListMap.empty)))
    def withoutMbFunction: Type = tpe.filterAnnotations(_.tpe.typeSymbol != mbFunctionClass)
  }

  implicit class RichTree(tree: Tree) {
    def isMbFunction: Boolean = tree.tpe.isMbFunction
  }


  // Support for transforming anonymous function parents
  object AnonymousFunctionSupport {
    def isTypicalParentList(parents: List[Type]) =
      if (parents.length == 2)
        AbstractFunctions.contains(parents(0).typeSymbol) &&
        parents(1).typeSymbol == SerializableClass
      else
        false

    def isTypicalDeclarationList(decls: List[Symbol]) =
      if (decls.length == 2)
        decls.exists(_.name == nme.CONSTRUCTOR) &&
        decls.exists(_.name == nme.apply)
      else
        false

    def tweakedParents(parents: List[Type]) = {
      assert(isTypicalParentList(parents))
      appliedType(tycon = abstractFunctionToAbstractMiniboxedTpe(parents(0).typeSymbol),
                  args  = parents(0).typeArgs) :: parents.tail
    }
  }

  // TODO: Add curried, tupled, etc...
  lazy val directMethodUpdate =
    Map(
      Function0Class.tpe.member(newTermName("apply")) -> MiniboxedFunction0Class.tpe.member(newTermName("apply")),
      Function1Class.tpe.member(newTermName("apply")) -> MiniboxedFunction1Class.tpe.member(newTermName("apply")),
      Function2Class.tpe.member(newTermName("apply")) -> MiniboxedFunction2Class.tpe.member(newTermName("apply")),
      Function0Class.tpe.member(newTermName("toString")) -> MiniboxedFunction0Class.tpe.member(newTermName("toString")),
      Function1Class.tpe.member(newTermName("toString")) -> MiniboxedFunction1Class.tpe.member(newTermName("toString")),
      Function2Class.tpe.member(newTermName("toString")) -> MiniboxedFunction2Class.tpe.member(newTermName("toString"))
    )

  lazy val directMethodSymbols = directMethodUpdate.keySet.toList

  lazy val nobridgeClass = global.rootMirror.getRequiredClass("miniboxing.runtime.nobridge")
  lazy val nobridgeTpe = nobridgeClass.tpe
}
