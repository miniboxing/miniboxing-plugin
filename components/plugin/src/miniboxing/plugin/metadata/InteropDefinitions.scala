//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2012-2014 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Vlad Ureche
//
package miniboxing.plugin
package metadata

import scala.tools.nsc.plugins.PluginComponent
import scala.collection.immutable.ListMap
import sun.org.mozilla.javascript.internal.FunctionObject

trait InteropDefinitions {
  this: PluginComponent =>

  import global._
  import definitions._

  lazy val mbFunctionClass = global.rootMirror.getRequiredClass("miniboxing.mbFunction")

  lazy val MiniboxedFunction0Class = global.rootMirror.getRequiredClass("miniboxing.runtime.MiniboxedFunction0")
  lazy val MiniboxedFunction1Class = global.rootMirror.getRequiredClass("miniboxing.runtime.MiniboxedFunction1")
  lazy val MiniboxedFunction2Class = global.rootMirror.getRequiredClass("miniboxing.runtime.MiniboxedFunction2")
  lazy val MiniboxedFunction0PolyTpe = PolyType(MiniboxedFunction0Class.typeParams, MiniboxedFunction0Class.tpe)
  lazy val MiniboxedFunction1PolyTpe = PolyType(MiniboxedFunction1Class.typeParams, MiniboxedFunction1Class.tpe)
  lazy val MiniboxedFunction2PolyTpe = PolyType(MiniboxedFunction2Class.typeParams, MiniboxedFunction2Class.tpe)
  lazy val Function0Class = global.definitions.FunctionClass(0)
  lazy val Function1Class = global.definitions.FunctionClass(1)
  lazy val Function2Class = global.definitions.FunctionClass(2)

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

  def flag_rewire_functionX: Boolean

  // Addons, not yet separated:

  implicit class RichType(tpe: Type) {
    def isMbFunction: Boolean = tpe.dealiasWiden.annotations.exists(_.tpe.typeSymbol == mbFunctionClass)
    def withMbFunction: Type = tpe.withAnnotations(List(Annotation.apply(mbFunctionClass.tpe, Nil, ListMap.empty)))
    def withoutMbFunction: Type = tpe.filterAnnotations(_.tpe.typeSymbol != mbFunctionClass)
  }

  implicit class RichTree(tree: Tree) {
    def isStorage: Boolean = tree.tpe.isMbFunction
  }
}
