trait Super[@specialized(Int) A] {
  def superb = 0
}
 
object Sub extends Super[Int] {
  super[Super].superb // okay
  super.superb        // okay   
  override def superb: Int = super.superb // okay
}
