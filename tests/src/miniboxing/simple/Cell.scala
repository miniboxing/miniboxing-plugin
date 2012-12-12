package miniboxing.simple

import miniboxing.plugin.minispec

class Cell[@minispec T : Manifest](t : T) {
  override def toString():String = {
    "Cell(" + t + ")" + t.##
  }
}

