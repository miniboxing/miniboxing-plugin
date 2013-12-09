package miniboxing.plugin

import scala.tools.nsc.plugins.PluginComponent

trait MiniboxAnnotationCheckers {
  this: MiniboxAdaptComponent =>

  import global._
  import minibox._

  object StorageAnnotationChecker extends AnnotationChecker{
    /** Check the annotations on two types conform. */
    override def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
//      println(tpe1)
//      println(tpe2)
      tpe1.dealiasWiden.hasAnnotation(StorageClass) == tpe2.dealiasWiden.hasAnnotation(StorageClass)
    }

    /** Modify the type that has thus far been inferred
     *  for a tree.  All this should do is add annotations. */
    override def addAnnotations(tree: Tree, tpe: Type): Type = tpe

    /** Decide whether this annotation checker can adapt a tree
     *  that has an annotated type to the given type tp, taking
     *  into account the given mode (see method adapt in trait Typers).*/
    override def canAdaptAnnotations(tree: Tree, mode: Int, pt: Type): Boolean = {
      //println("canAdaptAnnotations? " + tree)
      //!tree.isInstanceOf[TypeTree]
      false
    }

    /** Adapt a tree that has an annotated type to the given type tp,
     *  taking into account the given mode (see method adapt in trait Typers).
     *  An implementation cannot rely on canAdaptAnnotations being called
     *  before. If the implementing class cannot do the adaptiong, it
     *  should return the tree unchanged.*/
//    override def adaptAnnotations(tree: Tree, mode: Int, pt: Type): Tree = {
//    }
  }
}
