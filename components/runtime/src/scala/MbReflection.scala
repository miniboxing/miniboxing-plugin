//
//     _____   .__         .__ ____.                     .__ scala-miniboxing.org
//    /_   _\  |__|  ____  |__|\_  |__    _____  ___  ___|__|  ____    _____
//   / o\ /o \ |  | /    \ |  | |  __ \  /  ___\ \  \/  /|  | /    \  /  ___\
//  /    Y    \|  ||   |  \|  | |  \_\ \(  (_)  ) >    < |  ||   |  \(  /_/  )
//  \____|__  /|__||___|  /|__| |____  / \_____/ /__/\_ \|__||___|  / \___  /
//          \/          \/           \/                \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Vlad Ureche
//    * Nicolas Stucki
//
// Thanks to: @tixxit (Tom Switzer), @dlwh (David Hall) and @ichoran (Rex Kerr)
// for their very good feedback!
//
package scala

import scala.reflect.ClassTag

/**
 *  The `MiniboxingReflection` object allows reflecting on the type parameters
 *  of miniboxed classes. For example, given `class C[@miniboxed T]`, reflection
 *  can determine at run-time whether a type parameter is miniboxed, its
 *  instantiation and what type is used to store this value. These methods do
 *  not incur any overhead, since they are compiled away by the miniboxing
 *  plugin.
 *
 *  Let's see an example:
 *
 *  {{{
 *     scala> import MbReflection._
 *     import MbReflection._
 *
 *     scala> class C[@miniboxed T] {
 *          |   override def toString: String =
 *          |     s"C[T = ${reifiedType[T]}, miniboxed into a ${storageType[T]}]"
 *          | }
 *     defined class C
 *
 *     scala> new C[Int]
 *     res4: C[Int] = C[T = int, miniboxed into a long]
 *
 *     scala> new C[Unit]
 *     res5: C[Unit] = C[T = unit, miniboxed into a long]
 *
 *     scala> new C[Float]
 *     res6: C[Float] = C[T = float, miniboxed into a double]
 *  }}}
 *
 *  Yet, it's still possible to instantiate the class in an erased context,
 *  leading to suboptimal storage. Still, the miniboxing plugin will slap you
 *  on the wrist for doing so, and reflection will tell you you're doing it
 *  wrong:
 *
 *  {{{
 *     scala> def newC[T] = new C[T]
 *     <console>:11: warning: The following code could benefit from miniboxing
 *     specialization if the type parameter T of method newC would be marked
 *     as "@miniboxed T" (it would be used to instantiate miniboxed type
 *     parameter T of class C)
 *            def newC[T] = new C[T]
 *                          ^
 *     newC: [T]=> C[T]
 *
 *     scala> newC[Int]
 *     <console>:13: warning: The method newC would benefit from miniboxing
 *     type parameter T, since it is instantiated by a primitive type.
 *                   newC[Int]
 *                       ^
 *      res3: C[Int] = C[T = reference, miniboxed into a reference]
 *   }}}
 *
 *   Finally, there's a good use case for this, if the class is always supposed
 *   to be miniboxed:
 *
 *   {{{
 *      scala> class D[@miniboxed T] {
 *           |   assert(isMiniboxed[T], "Idiot!")
 *           | }
 *      define class D
 *
 *      scala> new D[String]
 *      java.lang.AssertionError: assertion failed: Idiot!
 *        at scala.Predef$.assert(Predef.scala:165)
 *        ... 34 elided
 *   }}}
 *
 *   The "isMiniboxed" method is also partially evaluated away by the compiler:
 *
 *   {{{
 *      scala> def foo[@miniboxed T]: Unit = {
 *           |   if (isMiniboxed[T])
 *           |     println("foo[miniboxed]")
 *           |   else
 *           |     println("foo[reference]")
 *           | }
 *      foo: [T]=> Unit
 *
 *      scala> foo[Byte]
 *      foo[miniboxed]
 *   }}}
 *
 *   In the low-level bytecode you will have:
 *
 *   {{{
 *      def foo(): Unit = println("foo[reference]")
 *      def foo_J(...): Unit = println("foo[miniboxed]")
 *      def foo_D(...): Unit = println("foo[miniboxed]")
 *   }}}
 *
 *   So there is 0 overhead in using the `isMiniboxed` method. Still,
 *   `reifiedType` and `storageType` are not partially evaluated and do incur
 *   a small overhead.
 */
object MbReflection {

  object SimpleType extends Enumeration {
    type SimpleType = Value
    val unit,
        boolean,
        byte,
        char,
        short,
        int,
        float,
        long,
        double,
        reference = Value
  }
  import SimpleType._

  /**
   *  Is this type parameter miniboxed?
   */
  def isMiniboxed[T]: Boolean = native

  /**
   *  If miniboxed, what is the actual type of a type parameter?
   *  It can be any of the [[SimpleType scala miniboxing types]].
   */
  def reifiedType[T]: SimpleType = native

  /**
   *  If miniboxed, what is the storage type of a type parameter?
   *
   *  The storage type of a miniboxed type parameter can be one of the
   *  following three types:
   *    * `Object` for instantiations with `AnyRef-based` types,
   *  erased generic types and value classes
   *    * `Long` for instantiations with `Unit`, `Boolean`, `Byte`,
   *  `Char`, `Short`, `Int` and `Long`
   *    * `Double` for instantiations with `Float` and `Double`
   */
  def storageType[T]: SimpleType = native

  // This should be hijacked by the miniboxing plugin to provide the actual implementations:
  private[this] def native = sys.error("The miniboxing plugin was not active during compilation.")

  // TODO: MbArray reflection!
}