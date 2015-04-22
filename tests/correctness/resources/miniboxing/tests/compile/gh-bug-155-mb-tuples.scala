object Test {
  def MbTuple1Constructor[@miniboxed X](x: X) = {
    val t1 = new Tuple1[X](x)
    val t2 = new Tuple1(1)
    val t3 = new Tuple1(1.1)
  }

  def MbTuple2constructor[@miniboxed X, @miniboxed Y](x: X, y: Y) = {
    val t1 = new Tuple2(1, 1.1)
    val t2 = new Tuple2(x, y)
    val t3 = new Tuple2(1, y)
    val t4 = new Tuple2(x, 1.1)

    val t6 = new Tuple2(1, 1.1)
    val t7 = new Tuple2(x, y)
    val t8 = new Tuple2(1, y)
    val t9 = new Tuple2(x, 1.1)
  }

  def MbTupleAccessors[@miniboxed X](x: X) = {
    val tuple1 = new Tuple1(x)
    val tuple2 = new Tuple2(x, x)

    val t1: X = tuple1._1
    val t2: X = tuple2._1
    val t3: X = tuple2._2

    val tuple3 = new Tuple1(1)
    val tuple4 = new Tuple2[Double, X](2.2, x)
    val t4: Int = tuple3._1
    val t5: Double = tuple4._1
    val t6: X = tuple4._2
  }
}
