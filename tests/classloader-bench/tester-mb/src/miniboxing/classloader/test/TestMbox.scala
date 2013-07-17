package miniboxing.classloader.test

import miniboxing.benchmarks.collection.immutable._
import miniboxing.runtime.MiniboxConstants._

object TestMboxing {
  def main(args: Array[String]): Unit = {
    println("Test Miniboxing")
    var time = System.currentTimeMillis()
    VectorFactory.newVector_J(UNIT, 0, 1, 0)
    VectorFactory.newVector_J(BOOLEAN, 0, 1, 0)
    VectorFactory.newVector_J(BYTE, 0, 1, 0)
    VectorFactory.newVector_J(CHAR, 0, 1, 0)
    VectorFactory.newVector_J(SHORT, 0, 1, 0)
    VectorFactory.newVector_J(INT, 0, 1, 0)
    VectorFactory.newVector_J(LONG, 0, 1, 0)
    VectorFactory.newVector_J(FLOAT, 0, 1, 0)
    VectorFactory.newVector_J(DOUBLE, 0, 1, 0)
    time -= System.currentTimeMillis()
    time = -time
    println("Time: " + time + "ms")
  }
}

/**********************************************************************************************************************
 * FACTORY C()DE ******************************************************************************************************
 **********************************************************************************************************************/
abstract class VectorFactoryInterface {
  def newVector_J[T$inst](T_TypeTag: Byte, start: Int, end: Int, focus: Int): Vector[T$inst]
}

class VectorFactoryInstance_class_J extends VectorFactoryInterface {
  def newVector_J[T$inst](T_TypeTag: Byte, start: Int, end: Int, focus: Int): Vector[T$inst] =
    // we need to trick Scala into loading up a symbol for Vector_class_J, which does not
    // have a pickled signature -- and thus should not be accessible at all, since it has
    // an empty "ScalaSig" signature :))
    (new Vector_class_J(T_TypeTag, start, end, focus)).asInstanceOf[Vector[T$inst]]
}

object VectorFactory {
  val fact = new Array[VectorFactoryInterface](10)

  @inline def newVector_J[T$inst](T_TypeTag: Byte, start: Int, end: Int, focus: Int): Vector[T$inst] =
    if (fact(T_TypeTag) == null)
      createFactoryAndObject(T_TypeTag)(T_TypeTag: Byte, start: Int, end: Int, focus: Int)
    else
      fact(T_TypeTag).newVector_J(T_TypeTag, start, end, focus)

  def createFactoryAndObject[T$inst](tag: Int)(T_TypeTag: Byte, start: Int, end: Int, focus: Int): Vector[T$inst] =
    try {
      val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(VectorFactory.this, verbose = sys.props.get("classloader.debug").isDefined)
      val clazz = classloader.findClass("miniboxing.classloader.test.VectorFactoryInstance_class_" + tag)
      val inst  = clazz.newInstance().asInstanceOf[VectorFactoryInterface]
      fact(tag) = inst
      fact(tag).newVector_J(T_TypeTag, start, end, focus)
    } catch {
      // TODO: What exactly do we want to catch?
      case other: NullPointerException =>
        fact(tag) = new VectorFactoryInstance_class_J()
        fact(tag).newVector_J(T_TypeTag, start, end, focus)
    }
}
