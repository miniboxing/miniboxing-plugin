package miniboxing.tests.compile



class ArgumentRewire[@miniboxed T] {
  def test(t: T) = {
    println(t)   // should be rewired to: println(minibox2box(t))
    t.hashCode   // should be rewired to: minibox2box(t).hashCode
    identity(t)  // should be rewired to: box2minibox(indentity(minibox2box(t)))
  }
}

class LocalRewire[@miniboxed T] {
  def test(t: T) = {
    val x = t    // x should be a Long instead of Tsp
    println(t)   // should be rewired to: println(minibox2box(x))
    t.hashCode   // should be rewired to: minibox2box(x).hashCode
    identity(t)  // should be rewired to: box2minibox(indentity(minibox2box(x)))
  }
}
