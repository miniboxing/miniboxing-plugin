package miniboxing.classloader.test

import miniboxing.benchmarks.collection.immutable._
import miniboxing.runtime.MiniboxConstants._

object TestMboxingClassloading {
  def main(args: Array[String]): Unit = {
    
    val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(TestMboxingClassloading.this, verbose = sys.props.get("classloader.debug").isDefined)
    warmup()

    // experiment:
    var i = 0
    var time = System.currentTimeMillis()
    while (i < 9) {
      classloader.findClass("miniboxing.benchmarks.collection.immutable.Vector_class_" + i)
      // if classes are dumped:
      //Class.forName("miniboxing.benchmarks.collection.immutable.Vector_class_" + i)
      i += 1
    }
    time -= System.currentTimeMillis()
    time = -time
    println(time)
  }
}

// Doesn't compile unless you dumped the classfiles:
/*
object TestMboxingDirect {
  def main(args: Array[String]): Unit = {
    println("Test Miniboxing (direct)")
    warmup()
    var time = System.currentTimeMillis()
    new Vector_class_0(UNIT, 0, 1, 0)
    new Vector_class_1(BOOLEAN, 0, 1, 0)
    new Vector_class_2(BYTE, 0, 1, 0)
    new Vector_class_3(CHAR, 0, 1, 0)
    new Vector_class_4(SHORT, 0, 1, 0)
    new Vector_class_5(INT, 0, 1, 0)
    new Vector_class_6(LONG, 0, 1, 0)
    new Vector_class_7(FLOAT, 0, 1, 0)
    new Vector_class_8(DOUBLE, 0, 1, 0)
    time -= System.currentTimeMillis()
    time = -time
    println("Time: " + time + "ms")
  }
}
*/

object TestMboxing {
  def main(args: Array[String]): Unit = {
    println("Test Miniboxing (2factory)")
    warmup()
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

object warmup {

  def apply(): Unit = {
    if (sys.props.get("classloader.warm").isDefined) {
      val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(warmup.this) 
      var i = 0
      while (i < 9) {
        classloader.findClass("miniboxing.classloader.test.Warmup_class_" + i)
        i += 1
      }
    }
  }

  class Warmup_class_J(T_TypeTag: Int) extends Warmup2_class_J(T_TypeTag)
  class Warmup2_class_J(T_TypeTag: Int) extends Warmup3_class_J(T_TypeTag)
  class Warmup3_class_J(T_TypeTag: Int) extends Warmup4_class_J(T_TypeTag)
  class Warmup4_class_J(T_TypeTag: Int) extends Warmup5_class_J(T_TypeTag)
  class Warmup5_class_J(T_TypeTag: Int) extends Warmup6_class_J(T_TypeTag)
  class Warmup6_class_J(T_TypeTag: Int) extends Warmup7_class_J(T_TypeTag)
  class Warmup7_class_J(T_TypeTag: Int) extends Warmup8_class_J(T_TypeTag)
  class Warmup8_class_J(T_TypeTag: Int) extends Warmup9_class_J(T_TypeTag)
  class Warmup9_class_J(T_TypeTag: Int) extends Warmup10_class_J(T_TypeTag)
  class Warmup10_class_J(T_TypeTag: Int) extends Warmup11_class_J(T_TypeTag)
  class Warmup11_class_J(T_TypeTag: Int) extends Warmup12_class_J(T_TypeTag)
  class Warmup12_class_J(T_TypeTag: Int) extends Warmup13_class_J(T_TypeTag)
  class Warmup13_class_J(T_TypeTag: Int) extends Warmup14_class_J(T_TypeTag)
  class Warmup14_class_J(T_TypeTag: Int) extends Warmup15_class_J(T_TypeTag)
  class Warmup15_class_J(T_TypeTag: Int) extends Warmup16_class_J(T_TypeTag)
  class Warmup16_class_J(T_TypeTag: Int) extends Warmup17_class_J(T_TypeTag)
  class Warmup17_class_J(T_TypeTag: Int) extends Warmup18_class_J(T_TypeTag)
  class Warmup18_class_J(T_TypeTag: Int) extends Warmup19_class_J(T_TypeTag)
  class Warmup19_class_J(T_TypeTag: Int) extends Warmup20_class_J(T_TypeTag)
  class Warmup20_class_J(T_TypeTag: Int) extends Warmup21_class_J(T_TypeTag)
  class Warmup21_class_J(T_TypeTag: Int) extends Warmup22_class_J(T_TypeTag)
  class Warmup22_class_J(T_TypeTag: Int) extends Warmup23_class_J(T_TypeTag)
  class Warmup23_class_J(T_TypeTag: Int) extends Warmup24_class_J(T_TypeTag)
  class Warmup24_class_J(T_TypeTag: Int) extends Warmup25_class_J(T_TypeTag)
  class Warmup25_class_J(T_TypeTag: Int) extends Warmup26_class_J(T_TypeTag)
  class Warmup26_class_J(T_TypeTag: Int) extends Warmup27_class_J(T_TypeTag)
  class Warmup27_class_J(T_TypeTag: Int) extends Warmup28_class_J(T_TypeTag)
  class Warmup28_class_J(T_TypeTag: Int) extends Warmup29_class_J(T_TypeTag)
  class Warmup29_class_J(T_TypeTag: Int) 
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
      val inst = clazz.newInstance().asInstanceOf[VectorFactoryInterface]
      fact(tag) = inst
      fact(tag).newVector_J(T_TypeTag, start, end, focus)
    } catch {
      // TODO: What exactly do we want to catch?
      case other: NullPointerException =>
        fact(tag) = new VectorFactoryInstance_class_J()
        fact(tag).newVector_J(T_TypeTag, start, end, focus)
    }
}
