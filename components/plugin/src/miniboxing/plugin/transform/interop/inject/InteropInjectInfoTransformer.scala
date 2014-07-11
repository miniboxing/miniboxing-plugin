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
package miniboxing
package plugin
package transform
package interop
package inject

import scala.tools.nsc.transform.InfoTransform

trait InteropInjectInfoTransformer extends InfoTransform {
  self: InteropInjectComponent =>

  import global._

  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    val res =
      if (flag_rewire_functionX && currentRun.compiles(sym)) {
        updatedType(tpe)
      } else
        tpe
//
//    if (res ne tpe)
//      println(beforeInteropInject(sym.defString) + "  " + res)
//    else if (sym.isMethod && currentRun.compiles(sym))
//      println(beforeInteropInject(sym.defString) + "  : no change")

    res
  }

  def updatedType(tpe: Type): Type =
    (tpe.withoutAnnotations match {
      case TypeRef(_, Function0Class, _) => tpe.withMbFunction
      case TypeRef(_, Function1Class, _) => tpe.withMbFunction
      case TypeRef(_, Function2Class, _) => tpe.withMbFunction
      case NullaryMethodType(res)        =>
        val nres = updatedType(res)
        if (nres eq res) tpe else NullaryMethodType(nres)
      case MethodType(args, res)         =>
        val nres = updatedType(res)
        if (nres eq res) tpe else MethodType(args, nres)
      case PolyType(targs, res)          =>
        val nres = updatedType(res)
        if (nres eq res) tpe else PolyType(targs, updatedType(res))
      case _ => tpe
    }).withAnnotations(tpe.annotations)
}