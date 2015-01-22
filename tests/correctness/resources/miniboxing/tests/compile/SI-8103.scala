package miniboxing.tests.compile.bugSI8103

object Test extends App {
    trait Foo[@specialized(Int) T] {
      def f1(t: T) { print("foo.f1"); f2(t) }
      def f2(t: T) { println(" foo.f2")}
    }
    trait Bar[T] extends Foo[T] {
       override def f2(t: T) = { print(" bar.f2" ); super.f2(t) }
    }
     
    class IntMixin() extends Foo[Int] with Bar[Int]
    new IntMixin().f1(0) // "foo.f1 foo.f2" WRONG
     
    class DoubleMixin() extends Foo[Double] with Bar[Double]
    new DoubleMixin().f1(2) // "foo.f1 bar.f2 foo.f2" OK
     
    class IntBar() extends Bar[Int]
    new IntBar().f1(0) // "foo.f1 bar.f2 foo.f2" OK
}
