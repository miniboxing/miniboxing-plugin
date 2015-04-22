import reflect.ClassTag

class C[@miniboxed T](val arr: MbArray[T]) {
  // empty:
  def this(size: Int) = this(MbArray.empty[T](size))
  // clone:
  def this(a: Array[T]) = this(MbArray.clone[T](a))

  // arraycopy:
  MbArray.arraycopy(arr, 0, arr, 0, 0)

  // Array:
  def newArray = new C(new Array[Int](10))

  // implicitly:
  implicit val x = 3
  implicitly[Int]

  // ClassTag:
  implicitly[ClassTag[Int]]

  // asInstanceOf/isInstanceOf:
  def foo(x: Any) =
    if (x.isInstanceOf[Int])
      println(x.asInstanceOf[Int])

  // arrowassoc:
  2 -> 3
}

object Test {
  def zoo[@specialized S](t: S) = t

  zoo[Int](3)

  def zar[@miniboxed S](t: S) = zoo[S](t)
}

