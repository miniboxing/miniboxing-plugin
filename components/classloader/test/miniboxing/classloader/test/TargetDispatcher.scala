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

class TargetDispatcher_J[T$sp](val t_J: Long, dispatcher: Dispatcher) extends TargetDispatcher[T$sp] {
  def t = ???
  def print: Unit = {
    System.out.println("print(" + t_J + ", " + dispatcher + ") by " + this.getClass.getName())
  }
}

class TargetDispatcher_L[T$sp](val t: T$sp) extends TargetDispatcher[T$sp] {
  def t_J = ???
  def print: Unit = System.out.println("print( " + t.toString + ") by " + this.getClass.getName())
}


/**********************************************************************************************************************
 * FACTORY C()DE ******************************************************************************************************
 **********************************************************************************************************************/
/*
 * That's a lot of bytecode for constructing the class.
 * TODO: Can we factor out some common functionalty?
 */
trait TargetDispatcherFactoryInterface {
  def newTargetDispatcher_J[T$inst](_t_J: Long, dispatcher: Dispatcher): TargetDispatcher[T$inst]
}

class TargetDispatcherFactoryInstance_J extends TargetDispatcherFactoryInterface {
  def newTargetDispatcher_J[T$inst](_t_J: Long, dispatcher: Dispatcher): TargetDispatcher[T$inst] = new TargetDispatcher_J(_t_J, dispatcher)
}

object TargetDispatcherFactory {
  val factories = new Array[TargetDispatcherFactoryInterface](10)

  def newTargetDispatcher_J[T$inst](_t_J: Long, dispatcher: Dispatcher): TargetDispatcher[T$inst] = {
    val tag = dispatcher.tag
    try {
      val fact = factories(tag)
      fact.newTargetDispatcher_J(_t_J, dispatcher)
    } catch {
      // factory creation is outside the critical path
      case _: NullPointerException =>
        try {
          val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(TargetDispatcherFactory.this)
          val clazz = classloader.findClass("miniboxing.classloader.test.TargetDispatcherFactoryInstance_" + tag)
          val inst  = clazz.newInstance().asInstanceOf[TargetDispatcherFactoryInterface]
          factories(tag) = inst
          newTargetDispatcher_J(_t_J, dispatcher)
        } catch {
//          case cnf: ClassNotFoundException =>
//            new TargetDispatcher_J[T$inst](_t_J, dispatcher)
          case other: Throwable => throw other
        }
    }
  }
}
