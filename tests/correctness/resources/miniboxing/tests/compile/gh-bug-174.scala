package miniboxing.tests.compile.bug174

class C[@miniboxed T] {
  try {
    println("hi")
  } catch {
    case e: Exception => println(e)
  }
}
