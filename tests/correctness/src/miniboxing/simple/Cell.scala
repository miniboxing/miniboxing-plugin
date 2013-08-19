package miniboxing.simple



class Cell[@miniboxed T : Manifest](t : T) {
  override def toString():String = {
    "Cell(" + t + ")" + t.##
  }
}

