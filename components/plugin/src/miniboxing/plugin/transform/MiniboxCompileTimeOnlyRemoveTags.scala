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
//    * Milos Stojanovic
//
package miniboxing.plugin
package transform
package hijack

import scala.tools.nsc.transform.InfoTransform

trait MiniboxCompileTimeOnlyRemoveTags extends InfoTransform {
  this: CompileTimeOnlyRemoveTagsComponent =>

  import global._
  import definitions._
  import scala.reflect.internal.Flags._

  def isMiniboxedAnnotation(ann: Option[Annotation]): Boolean = {
    ann.map(_.args) match {
      case Some(List(Literal(Constant(msg)))) =>
        // TODO: Check the message is actually a miniboxing-issued message
        // TODO: Check the miniboxing plugin version and warn in case of mismatch
        true
      case _ => false
    }
  }

  def transformInfo(sym: Symbol, tpe: Type): Type = {
    if (sym.name == nme.CONSTRUCTOR &&
        (sym.owner.isClass || sym.owner.isTrait) &&
        sym.hasAnnotation(CompileTimeOnlyClass) &&
        isMiniboxedAnnotation(sym.getAnnotation(CompileTimeOnlyClass)))
      sym.removeAnnotation(CompileTimeOnlyClass)
    tpe
  }
}
