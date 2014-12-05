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
package mbarray

import scala.tools.nsc.transform.TypingTransformers
import scala.reflect.internal.Phase

trait MbArrayTreeTransformer extends TypingTransformers {
  self: MbArrayOptimizeComponent =>

  import global._

  class OptimizePhase(prev: Phase) extends StdPhase(prev) {
    override def name = MbArrayTreeTransformer.this.phaseName
    override def checkable = true
    def apply(unit: CompilationUnit): Unit =
      if (flag_rewire_mbarray)
        if (!minibox.flag_two_way) {
          global.reporter.warning(unit.body.pos, "Optimizing `MbArray` is only possible if you allow the plugin to use both {long, double} encodings (-Y:minibox:Ytwo-way). `MbArray` will be generic and will box.")
        } else
          afterMbArrayOptimize(new OptimizeTransformer(unit).transformUnit(unit))
      else
        global.reporter.warning(unit.body.pos, "Heads-up: Optimizing `MbArray` is disabled, thus all `MbArrays` will box.")
  }

  class OptimizeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    import global._
    import minibox._

  }
}