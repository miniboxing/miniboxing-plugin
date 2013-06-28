package miniboxing.tests.compile.nested

import miniboxing.plugin.minispec

class C[@minispec T](c: T) {
  class D[@minispec U](d: U)
}
