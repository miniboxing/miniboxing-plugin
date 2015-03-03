object Test2 {

  implicit class RichX[@miniboxed X](val x: X) extends AnyVal {
    def f = 0
  }

  def main(args: Array[String]) = {
    val i = 0
    print(i.f)
  }
}
