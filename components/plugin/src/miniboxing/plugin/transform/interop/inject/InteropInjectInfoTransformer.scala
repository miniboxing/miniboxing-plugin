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
package miniboxing
package plugin
package transform
package interop
package inject

import scala.tools.nsc.transform.InfoTransform

trait InteropInjectInfoTransformer extends InfoTransform {
  self: InteropInjectComponent =>

  import global._
  import definitions.ByNameParamClass

  def isDelambdafyParam(sym: Symbol) =
    sym.owner.isAnonymousFunction && sym.isValueParameter

  override def transformInfo(sym: Symbol, tpe: Type): Type = {

    val res =
      if (flag_rewire_functionX_values && (sym.sourceFile != null) && !isDelambdafyParam(sym)) {
        updatedType(NoPosition, tpe)
      } else {
        tpe
      }

    res
  }

  def updatedType(pos: Position, tpe: Type): Type =
    (tpe.withoutAnnotations match {
      case TypeRef(pre, fun, targs) if FunctionSyms.contains(fun) =>
        if (tpe.hasApiAnnotation || targs.last.hasApiAnnotation) typeRef(pre, fun, targs.map(_.withoutApiAnnotations)) else tpe.withMbFunction
      case TypeRef(pre, ByNameParamClass, List(targ)) =>
        if (targ.hasApiAnnotation) typeRef(pre, ByNameParamClass, List(targ.withoutApiAnnotations)) else tpe.withMbFunction
      case NullaryMethodType(res)        =>
        val nres = updatedType(pos, res)
        if (nres eq res) tpe else NullaryMethodType(nres)
      case MethodType(args, res)         =>
        val nres = updatedType(pos, res)
        if (nres eq res) tpe else MethodType(args, nres)
      case PolyType(targs, res)          =>
        val nres = updatedType(pos, res)
        if (nres eq res) tpe else PolyType(targs, updatedType(pos, res))
      case _ =>
        tpe.withoutAnnotations
    }).withAnnotations(tpe.annotations.filter(_.tpe.typeSymbol != apiClass))
}