package miniboxing.tests.compile.bug53

object Test3 {
  class BUG53outer[@miniboxed T]() {
    class BUG53inner[@miniboxed T]()
  }
}
