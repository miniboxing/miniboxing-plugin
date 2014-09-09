package miniboxing.tests.compile.bug126

class Config

class GraphBuilder[@miniboxed Id](b: Config) {
  val a = b // should trigger the error
  val c = b // should NOT trigger the error
}
