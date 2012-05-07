package simple

import plugin.minispec

class Pair[@minispec T1,@minispec T2](t1: T1, t2: T2) {
  def a = t1
  def b = t2
  var c = t1
}

