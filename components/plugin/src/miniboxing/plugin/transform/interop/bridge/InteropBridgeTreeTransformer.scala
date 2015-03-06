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
package transform
package interop
package bridge

import scala.tools.nsc.transform.TypingTransformers
import infrastructure.TreeRewriters
import scala.reflect.internal.Flags._

trait InteropBridgeTreeTransformer extends TreeRewriters with ScalacCrossCompilingLayer {
  self: InteropBridgeComponent =>

  import global._
  import interop._

  class BridgePhase(prev: Phase) extends StdPhase(prev) {
    override def name = InteropBridgeTreeTransformer.this.phaseName
    override def checkable = true
    def apply(unit: CompilationUnit): Unit = {
      afterInteropBridge(new BridgeTransformer(unit).transformUnit(unit))
    }
  }

  class BridgeTransformer(unit: CompilationUnit) extends TreeRewriter(unit) with ScalacVersion {

    import global._
    import definitions.BridgeClass

    def hasMbFunction(defdef: DefDef) =
      defdef.vparamss.flatten.exists(_.tpt.tpe.isMbFunction) || defdef.tpt.isMbFunction

    def setNoErasureBridge(sym: Symbol) =
      // Erasure generates the last iteration of bridges, the ones that take Object.
      // Yet, in some cases, there is no need to generate these bridges, since they
      // interfere with the JVM bytecode invariants. Instead of preventing erasure
      // from creating them in the first place (which can't be done in a plugin)
      // we remove them later, in the `mb-tweakerasure` phase
      sym.addAnnotation(AnnotationInfo(nobridgeTpe, Nil, Nil))

    protected def rewrite(tree: Tree): Result = {
      tree match {
        case defdef: DefDef => // if hasStorage(defdef) =>

          def hasMbFunction(sym: Symbol) = sym.paramss.flatten.exists(_.tpe.isMbFunction) || sym.tpe.finalResultType.isMbFunction

          def sameResultEncoding(as: Symbol) = (s: Symbol) => s.tpe.finalResultType.isMbFunction == as.info.finalResultType.isMbFunction

          val preOverrides = beforeInteropBridgeNext(defdef.symbol.allOverriddenSymbols).flatMap(_.alternatives)
          val postOverrides = afterInteropBridgeNext(defdef.symbol.allOverriddenSymbols).flatMap(_.alternatives).filter(sameResultEncoding(defdef.symbol))

          val bridgeSyms = preOverrides.filterNot(postOverrides.contains)

          def filterBridges(bridges: List[Symbol]): List[Symbol] = bridges match {
            case Nil => Nil
            case sym :: tail =>
              val overs = afterInteropBridgeNext(sym.allOverriddenSymbols).flatMap(_.alternatives).filter(sameResultEncoding(sym))
              val others = tail filterNot (overs.contains)
//              println()
//              println("sym: " + sym)
//              println("tail: " + tail)
//              println("overs: " + overs)
//              println("others: " + others)
              sym :: filterBridges(others)
          }

//          println()
//          println("filtering bridges for: " + defdef.symbol + " in " + defdef.symbol.owner)
//          println("bridgeSyms: " + bridgeSyms)

          val bridgeSymsFiltered = filterBridges(bridgeSyms)

//          println("filtered bridges for: " + defdef.symbol + " in " + defdef.symbol.owner + "::: " + bridgeSymsFiltered)

          val bridges: List[Tree] =
            for (sym <- bridgeSymsFiltered) yield {
              val local = defdef.symbol
              val decls = local.owner.info.decls

              // bridge symbol:
              val bridge = local.cloneSymbol
              bridge.setInfo(local.owner.info.memberInfo(sym).cloneInfo(bridge))
              bridge.addAnnotation(BridgeClass)
              if (decls != EmptyScope) decls enter bridge

              // bridge tree:
              val bridgeRhs0 = gen.mkMethodCall(gen.mkAttributedRef(local), bridge.typeParams.map(_.tpeHK), bridge.info.params.map(Ident))
              val bridgeRhs1 = atOwner(bridge)(localTyper.typed(bridgeRhs0))
              val bridgeDef = newDefDef(bridge, bridgeRhs1)() setType NoType
              mmap(bridgeDef.vparamss)(_ setType NoType)

              // transform RHS of the defdef + typecheck
              val bridgeDef2 = localTyper.typed(bridgeDef)

              if (hasMbFunction(bridge))
                setNoErasureBridge(bridge)

              bridgeDef2
            }

          if (hasMbFunction(defdef.symbol))
            setNoErasureBridge(defdef.symbol)

          val defdef2 = localTyper.typed(deriveDefDef(defdef){rhs => super.atOwner(defdef.symbol)(super.transform(rhs))})

          Multi(defdef2 :: bridges)
        case _ =>
          Descend
      }
    }
  }
}