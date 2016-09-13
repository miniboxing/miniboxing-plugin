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
        msg.toString.contains("was compiled with the miniboxing plugin and can only be called by code also " +
                              "compiled with the miniboxing plugin (see scala-miniboxing.org/compatibility)")
      case _ => false
    }
  }

  def transformInfo(sym: Symbol, tpe: Type): Type = {
    var removeAnnotation = false

    if (sym.name == nme.CONSTRUCTOR &&
       (sym.owner.isClass || sym.owner.isTrait) &&
       (sym.owner.typeParams.exists(_.hasAnnotation(MinispecClass))))
      removeAnnotation = true

    if ((sym.isMethod) &&
        (sym.hasAnnotation(CompileTimeOnlyClass)) &&
        (isMiniboxedAnnotation(sym.getAnnotation(CompileTimeOnlyClass))))
      removeAnnotation = true

    if (removeAnnotation)
      sym.removeAnnotation(CompileTimeOnlyClass)

    tpe
  }
}
