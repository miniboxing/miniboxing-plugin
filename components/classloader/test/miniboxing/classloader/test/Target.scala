package miniboxing.classloader.test

import scala.collection.mutable.WeakHashMap

trait Target[T] {
  def t: T
  def t_J: Long
  def print: Unit
}

class Target_J[T$sp](val t_J: Long, T_TypeTag: Byte) extends Target[T$sp] {
  def t = ???
  def print: Unit = {
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
}

class Target_L[T$sp](val t: T$sp) extends Target[T$sp] {
  def t_J = ???
  def print: Unit = System.out.println("print( " + t.toString + ") by " + this.getClass.getName())
}

/*
 * That's a lot of bytecode for constructing the class.
 * TODO: Can we factor out some common functionalty?
 */
trait TargetFactoryInterface {
  def newTarget_J[T$inst](_t_J: Long, T_TypeTag: Byte): Target[T$inst]
}

class TargetFactoryInstance_J extends TargetFactoryInterface {
  def newTarget_J[T$inst](_t_J: Long, T_TypeTag: Byte): Target[T$inst] = new Target_J(_t_J, T_TypeTag)
}

object TargetFactory {
  val factories = new Array[TargetFactoryInterface](10)

  def newTarget_J[T$inst](_t_J: Long, T_TypeTag: Byte): Target[T$inst] = {
    try {
      val fact = factories(T_TypeTag)
      fact.newTarget_J(_t_J, T_TypeTag)
    } catch {
      // factory creation is outside the critical path
      case _: NullPointerException =>
        try {
          val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(TargetFactory.this)
          val clazz = classloader.findClass("miniboxing.classloader.test.TargetFactoryInstance_" + T_TypeTag)
          val inst  = clazz.newInstance().asInstanceOf[TargetFactoryInterface]
          factories(T_TypeTag) = inst
          newTarget_J(_t_J, T_TypeTag)
        } catch {
//          case cnf: ClassNotFoundException =>
//            new Target_J[T$inst](_t_J, T_TypeTag)
          case other => throw other
        }
    }
  }
}
