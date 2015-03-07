final class Foo[@miniboxed A] private (x: A)
object Foo { def apply[@miniboxed A](x: A) = new Foo[A](x) } 
