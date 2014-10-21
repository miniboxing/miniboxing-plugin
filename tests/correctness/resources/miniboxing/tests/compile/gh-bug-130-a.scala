package miniboxing.tests.compile.bug130a

trait KeyObserver[@miniboxed -A] {
  def keyUp(key: A) : Unit = {
    println((new Exception()).getStackTrace().toList.take(4).mkString("in ", "\n\tat ", "\n"))
  }
}
object NoKeyObserver extends KeyObserver[Any]

trait TestX[@miniboxed A] {
  def lala : A
  def obs : KeyObserver[A]

  def test() = obs.keyUp(lala)
}

object Test {
  def main(args: Array[String]): Unit = {
    new TestX[Int]{ def lala = 3; def obs = NoKeyObserver }.test()
    new TestX[Any]{ def lala = 3; def obs = NoKeyObserver }.test()
    new TestX[Int]{ def lala = 3; def obs = new KeyObserver[Any]{} }.test()
    new TestX[Int]{ def lala = 3; def obs = new KeyObserver[Int]{} }.test()
    new TestX[Any]{ def lala = 3; def obs = new KeyObserver[Any]{} }.test()
  }
}
