package miniboxing.classloader.test

object TestMboxing {
  def main(args: Array[String]): Unit = {
    val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(TestMboxing.this, verbose = sys.props.get("classloader.debug").isDefined)
    
    // warmup:
    var i = 0
    while (i < 9) {
      classloader.findClass("miniboxing.classloader.test.Warmup_class_" + i)
      i += 1
    }

    // experiment:
    i = 0
    //println("START-----------------------------------------------------------------------------------------------")
    var time = System.currentTimeMillis()
    while (i < 9) {
      classloader.findClass("miniboxing.benchmarks.collection.immutable.Vector_class_" + i)
      // if classes are dumped:
      //Class.forName("miniboxing.benchmarks.collection.immutable.Vector_class_" + i)
      i += 1
    }
    time -= System.currentTimeMillis()
    //println("STOP------------------------------------------------------------------------------------------------")
    time = -time
    println(time)
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
