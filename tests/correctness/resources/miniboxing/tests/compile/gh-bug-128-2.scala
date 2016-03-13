trait A[@miniboxed K, @miniboxed V] { 
  def doStuff(k: K, v: V): Unit = println("Hi - I'm calling doStuff in A")
}

trait B[@miniboxed V] extends A[Int, V] { 
  override def doStuff(i: Int, v: V): Unit = println("Hi - I'm calling doStuff in B")
}

object Run {

  def main(args: Array[String]): Unit = {
    val a = new A[Long, Float]() {}
    a.doStuff(1L, 0.1F)

    val b = new B[Double]() {}
    b.doStuff(1, 0.1)

    delegate(a, 1L, 0.1F)
    delegate(b, 1, 0.1)
  }

  def delegate[@miniboxed K, @miniboxed V](a: A[K, V], k: K, v: V): Unit = a.doStuff(k, v)
}
