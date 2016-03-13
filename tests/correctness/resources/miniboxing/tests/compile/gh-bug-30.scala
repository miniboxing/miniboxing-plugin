trait Base1[@miniboxed T, S] {
  def overrideMe(t: T, s: S): T = t
}
 
trait Base2[@miniboxed T, @miniboxed S] {
  def overrideMe(t: T, s: S): T = t
}
 
class LongIsBase1andBase2 extends Base1[Long, Long] with Base2[Long, Long] {
  override def overrideMe(t: Long, s: Long): Long = s
}
 
object Test extends App {
  val long = new LongIsBase1andBase2
  val base1: Base1[Long, Long] = long
  val base2: Base2[Long, Long] = long
  println(long.overrideMe(1,2))
  println(base1.overrideMe(1,2))
  println(base2.overrideMe(1,2)) 
}
