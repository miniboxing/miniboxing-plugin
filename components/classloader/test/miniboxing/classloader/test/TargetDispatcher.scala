package miniboxing.classloader.test

import scala.collection.mutable.WeakHashMap

/**********************************************************************************************************************
 * DISPATCHER C()DE ***************************************************************************************************
 **********************************************************************************************************************/
trait Dispatcher {
  def tag: Int
  def toString(l: Long): String
}

object IntDispatcher extends Dispatcher {
  def tag = 0
  def toString(l: Long): String = l.toInt.toString
  override def toString = "IntDispatcher"
}

object LongDispatcher extends Dispatcher {
  def tag = 1
  def toString(l: Long): String = l.toInt.toString
  override def toString = "LongDispatcher"
}

/**********************************************************************************************************************
 * CLASS C()DE ********************************************************************************************************
 **********************************************************************************************************************/
trait TargetDispatcher[T] {
  def t: T
  def t_J: Long
  def print: Unit
}

class TargetDispatcher_class_J[T$sp](val t_J: Long, dispatcher: Dispatcher) extends TargetDispatcher[T$sp] {
  def t = ???
  def print: Unit = {
    System.out.println("print(" + t_J + ", " + dispatcher + ") by " + this.getClass.getName())
  }
}

/**********************************************************************************************************************
 * FACTORY C()DE ******************************************************************************************************
 **********************************************************************************************************************/
/*
 * That's a lot of bytecode for constructing the class.
 * TODO: Can we factor out some common functionalty?
 */
abstract class TargetDispatcherFactoryInterface {
  def newTargetDispatcher_J[T$inst](t_J: Long, dispatcher: Dispatcher): TargetDispatcher[T$inst]
}

class TargetDispatcherFactoryInstance_class_J extends TargetDispatcherFactoryInterface {
  def newTargetDispatcher_J[T$inst](t_J: Long, dispatcher: Dispatcher): TargetDispatcher[T$inst] = new TargetDispatcher_class_J(t_J, dispatcher)
}

object TargetDispatcherFactory {
  val fact = new Array[TargetDispatcherFactoryInterface](10)

  @inline def newTargetDispatcher_J[T$inst](t_J: Long, disp: Dispatcher): TargetDispatcher[T$inst] =
    if (fact(disp.tag) == null)
      createFactoryAndObject(disp.tag)(t_J, disp)
    else
      fact(disp.tag).newTargetDispatcher_J(t_J, disp)

  def createFactoryAndObject[T$inst](tag: Int)(t_J: Long, disp: Dispatcher): TargetDispatcher[T$inst] =
    try {
      val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(TargetDispatcherFactory.this)
      val clazz = classloader.findClass("miniboxing.classloader.test.TargetDispatcherFactoryInstance_class_" + tag)
      val inst  = clazz.newInstance().asInstanceOf[TargetDispatcherFactoryInterface]
      fact(tag) = inst
      fact(tag).newTargetDispatcher_J(t_J, disp)
    } catch {
      // TODO: What exactly do we want to catch?
      case other: Throwable =>
        fact(tag) = new TargetDispatcherFactoryInstance_class_J()
        fact(tag).newTargetDispatcher_J(t_J, disp)
    }
}
