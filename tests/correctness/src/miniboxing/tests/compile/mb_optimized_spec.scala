package miniboxing.tests.compile



class OptMeee[@miniboxed X, Y]{
  def yfoo1(x: X, y: Y): X = yfoo1(x, y)
  def yfoo2(y: Y): X = yfoo2(y)
  def nfoo3(y: Y): Y = nfoo3(y)
  def nfoo4(xs: List[X]): Array[X] = nfoo4(xs)
}
