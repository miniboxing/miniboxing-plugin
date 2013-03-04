package miniboxing.classloader.test

import scala.collection.mutable.WeakHashMap

trait Target[T] {
  def t: T
  def t_J: Long
  def print: Unit
}

class Target_J[T$sp](val t_J: Long, val T_TypeTag: Byte) extends Target[T$sp] {
  def t = ???
  def print: Unit = System.out.println("print(" + t_J + ", " + T_TypeTag + ") by " + this.getClass.getName())
}

class Target_L[T$sp](val t: T$sp) extends Target[T$sp] {
  def t_J = ???
  def print: Unit = System.out.println("print( " + t.toString + ") by " + this.getClass.getName())
}

object TargetFactory {
  val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(TargetFactory.this)

  def newTarget_J[T$inst](_t_J: Long, _T_TypeTag: Byte): Target[T$inst] = {
    try {
      val clazz = classloader.findClass("miniboxing.classloader.test.Target_" + _T_TypeTag)
      val const = clazz.getConstructor(classOf[Long], classOf[Byte])
      val inst  = const.newInstance(_t_J: java.lang.Long, _T_TypeTag: java.lang.Byte)
      inst.asInstanceOf[Target[T$inst]]
    } catch {
      case cnf: ClassNotFoundException =>
        ??? // new Target_J[T$inst](_t_J, _T_TypeTag)
    }
  }
}
