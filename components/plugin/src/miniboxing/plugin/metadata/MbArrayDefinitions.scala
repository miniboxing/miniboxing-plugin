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

trait MbArrayDefinitions {
  this: PluginComponent =>

  import global._
  import definitions._

  lazy val MbArrayClass   = global.rootMirror.getRequiredClass("scala.MbArray")
  lazy val MbArrayModule  = global.rootMirror.getRequiredModule("scala.MbArray")
  lazy val MbArray_apply  = definitions.getMember(MbArrayClass, newTermName("apply"))
  lazy val MbArray_update = definitions.getMember(MbArrayClass, newTermName("update"))
  lazy val MbArray_empty  = definitions.getMember(MbArrayModule, newTermName("empty"))
  lazy val MbArray_clone  = definitions.getMember(MbArrayModule, newTermName("clone")).
                              // filter Object.clone out, we don't want that:
                              alternatives.find(_.owner == MbArrayModule.moduleClass).get

  // optimized alternatives:
  lazy val MbArrayOpts    = global.rootMirror.getRequiredModule("miniboxing.runtime.array.MbArrayOpts")
  lazy val MbArrayOpts_apply: Map[Symbol, Symbol] =
    Map(LongClass   -> definitions.getMember(MbArrayOpts, newTermName("mbArray_apply_J")),
        DoubleClass -> definitions.getMember(MbArrayOpts, newTermName("mbArray_apply_D")))
  lazy val MbArrayOpts_update: Map[Symbol, Symbol] =
    Map(LongClass   -> definitions.getMember(MbArrayOpts, newTermName("mbArray_update_J")),
        DoubleClass -> definitions.getMember(MbArrayOpts, newTermName("mbArray_update_D")))
  lazy val MbArrayOpts_alternatives: Map[Symbol, Map[Symbol, Symbol]] =
    Map(MbArray_empty ->
          Map(LongClass   -> definitions.getMember(MbArrayOpts, newTermName("mbArray_empty_J")),
              DoubleClass -> definitions.getMember(MbArrayOpts, newTermName("mbArray_empty_D"))),
        MbArray_clone ->
          Map(LongClass   -> definitions.getMember(MbArrayOpts, newTermName("mbArray_clone_J")),
              DoubleClass -> definitions.getMember(MbArrayOpts, newTermName("mbArray_clone_D"))))
}
