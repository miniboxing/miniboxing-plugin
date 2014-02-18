package miniboxing.classloader.test

import scala.collection.mutable.WeakHashMap


/**********************************************************************************************************************
 * CLASS C()DE ********************************************************************************************************
 **********************************************************************************************************************/
// NOTE: This is the exact miniboxing encoding:

trait TargetTrait[T] {
  def TargetTrait_T_TypeTag: Byte
  def printTrait(): Unit
}

// Make sure we specialize trait implementations
trait TargetTrait_class_J[T] extends TargetTrait[T] {
  def printTrait(): Unit = System.out.println("printTrait: " + (new Exception()).getStackTrace()(0).getClassName())
}

trait TargetSuper[T] {
  def printSuper(): Unit
}

// Make sure we specialize superclasses
class TargetSuper_class_J[T](T_TypeTag: Byte) extends TargetSuper[T] {
  def printSuper(): Unit = System.out.println("printSuper: " + getClass().getSuperclass().getName())
}

trait Target[T] extends AnyRef with TargetSuper[T] with TargetTrait[T]{
  def t: T
  def t_J: Long
  def print(): Unit
  def printInnerClass(): Unit
}

// Make sure we specialize classes
class Target_class_J[T$sp](val t_J: Long, T_TypeTag: Byte) extends TargetSuper_class_J[T$sp](T_TypeTag) with TargetTrait_class_J[T$sp] with Target[T$sp] {
  def TargetTrait_T_TypeTag: Byte = T_TypeTag
  def t = ???
  def print(): Unit = {
    System.out.println("print(" + t_J + ", " + T_TypeTag + ") by " + this.getClass.getName())
    val x = T_TypeTag match {
      case 1 => 1
      case 2 => 2
      case 3 => 3
      case 4 => 4
      case 5 => 5
      case _ => 6
    }
    ()
  }
  def printInnerClass(): Unit = {
    class `$anon` extends Function0[String]{
      def apply(): String = (new Exception()).getStackTrace()(0).getClassName()
    }
    System.out.println("printInnerClass: " + (new `$anon`: Function0[String]).apply)
  }
}

/**********************************************************************************************************************
 * FACTORY C()DE ******************************************************************************************************
 **********************************************************************************************************************/
/*
 * That's a lot of bytecode for constructing the class.
 * TODO: Can we factor out some common functionalty?
 */
abstract class TargetFactoryInterface {
  def newTarget_J[T$inst](_t_J: Long, T_TypeTag: Byte): Target[T$inst]
}

class TargetFactoryInstance_class_J extends TargetFactoryInterface {
  def newTarget_J[T$inst](_t_J: Long, T_TypeTag: Byte): Target[T$inst] =
    new Target_class_J(_t_J, T_TypeTag)
}

object TargetFactory {
  val fact = new Array[TargetFactoryInterface](10)

  @inline def newTarget_J[T$inst](t_J: Long, T_TypeTag: Byte): Target[T$inst] =
    if (fact(T_TypeTag) == null)
      createFactoryAndObject(T_TypeTag)(t_J, T_TypeTag)
    else
      fact(T_TypeTag).newTarget_J(t_J, T_TypeTag)

  def createFactoryAndObject[T$inst](tag: Int)(t_J: Long, T_TypeTag: Byte): Target[T$inst] =
    try {
      val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(TargetFactory.this)
      val clazz = classloader.findClass("miniboxing.classloader.test.TargetFactoryInstance_class_" + tag)
      val inst  = clazz.newInstance().asInstanceOf[TargetFactoryInterface]
      fact(tag) = inst
      fact(tag).newTarget_J(t_J, T_TypeTag)
    } catch {
      // TODO: What exactly do we want to catch?
      case other: NullPointerException =>
        fact(tag) = new TargetFactoryInstance_class_J()
        fact(tag).newTarget_J(t_J, T_TypeTag)
    }
}
