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

trait MbArrayDefinitions {
  this: MbArrayOptimizeComponent =>

  import global._
  import definitions._

  lazy val MbArrayClass   = global.rootMirror.getRequiredClass("scala.MbArray")
  lazy val MbArrayModule  = global.rootMirror.getRequiredClass("scala.MbArray")
  lazy val MbArray_apply  = definitions.getMember(MbArrayClass, newTermName("apply"))
  lazy val MbArray_update = definitions.getMember(MbArrayClass, newTermName("update"))
  lazy val MbArray_empty  = definitions.getMember(MbArrayModule, newTermName("empty"))
  lazy val MbArray_clone  = definitions.getMember(MbArrayModule, newTermName("clone"))

  // optimized alternatives:
  lazy val MbArrayOpts    = global.rootMirror.getRequiredClass("miniboxing.runtime.array.MbArrayOpts")

}
