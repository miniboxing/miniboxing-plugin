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
package transform
package commit

import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.Phase

trait PrepareTreeTransformer extends TypingTransformers {
  self: PrepareComponent =>

  import global._

  class PreparePhaseImpl(prev: Phase) extends StdPhase(prev) {
    override def name = PrepareTreeTransformer.this.phaseName
    def apply(unit: CompilationUnit): Unit = {
//      val tree = afterPrepare((new TreeAdapters().adapt(unit))
//      tree.foreach(node => assert(node.tpe != null, node))
    }
  }
}
