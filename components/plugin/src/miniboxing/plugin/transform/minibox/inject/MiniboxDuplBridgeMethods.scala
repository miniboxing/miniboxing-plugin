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
//    * Cristian Talau
//
package miniboxing.plugin
package transform
package minibox
package inject

import scala.tools.nsc.transform.TypingTransformers

trait MiniboxDuplBridgeMethods extends TypingTransformers with ScalacCrossCompilingLayer {
  self: MiniboxInjectComponent =>

  import global._

  class BridgeTransformer(unit: CompilationUnit) extends TreeRewriter(unit) {

    import global._
    import definitions.BridgeClass

    def addBridges(owner: Symbol, tree: Tree): Tree = tree match {
      case d: DefDef =>
        val d2 = deriveDefDef(d)(rhs => atOwner(d.symbol)(transform(rhs)))
        val d3 = atOwner(owner)(localTyper.typed(d2))
        d3
      case v: ValDef =>
        val v2 = deriveValDef(v)(rhs => atOwner(v.symbol)(transform(rhs)))
        val v3 = atOwner(owner)(localTyper.typed(v2))
        v3
      case _ =>
        global.reporter.warning(tree.pos, "[internal miniboxing plugin error] Incorrect tree was fed into the bridge tool: " + tree)
        atOwner(owner)(transform(tree))
    }

    def hasStorage(defdef: DefDef) =
      defdef.vparamss.flatten.exists(_.tpt.tpe.isStorage) || defdef.tpt.isStorage

    protected def rewrite(tree: Tree): Result = {
      tree match {

        case defdef: DefDef if hasStorage(defdef) && flag_create_local_specs =>
          val local = defdef.symbol
          val decls = local.owner.info.decls

          // bridge symbol:
          val bridge = local.cloneSymbol
          beforeMiniboxInject(
            // so it gets transformed
            bridge.setInfo(local.tpe.cloneInfo(bridge).withoutStorageDeep)
          )

          // adding it to the new class:
          if (decls != EmptyScope) {
//            decls unlink local
            decls enter bridge
          }

          local.name = localMethodName(local)

          // bridge tree:
          val bridgeRhs0 = gen.mkMethodCall(gen.mkAttributedIdent(local), bridge.typeParams.map(_.tpeHK), bridge.info.params.map(Ident))
          val bridgeRhs1 = atOwner(bridge)(localTyper.typed(bridgeRhs0))
          val bridgeDef = newDefDef(bridge, bridgeRhs1)() setType NoType
          mmap(bridgeDef.vparamss)(_ setType NoType)

          // transform RHS of the defdef + typecheck
          val defdef2 = localTyper.typed(deriveDefDef(defdef){rhs => super.transform(rhs)})
          val bridgeDef2 = localTyper.typed(bridgeDef)

          Multi(List(defdef2, bridgeDef2))

        case _ =>
          Descend
      }
    }
  }
}