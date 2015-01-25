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
package minibox
package bridge

import scala.tools.nsc.transform.TypingTransformers
import infrastructure.TreeRewriters

trait MiniboxBridgeTreeTransformer extends TreeRewriters with ScalacCrossCompilingLayer {
  self: MiniboxBridgeComponent =>

  import global._
  import minibox._

  // TODO: We don't need this class -- remove it.
  class BridgePhase(prev: Phase) extends StdPhase(prev) {
    override def name = MiniboxBridgeTreeTransformer.this.phaseName
    override def checkable = true
    def apply(unit: CompilationUnit): Unit = ()
  }
}