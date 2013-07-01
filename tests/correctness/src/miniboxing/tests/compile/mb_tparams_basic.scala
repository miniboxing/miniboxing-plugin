package miniboxing.tests.compile.tparams

import miniboxing.plugin.minispec

class TParams1[@minispec T] {
  def foo[X](t: T, x: X) = 12
}
