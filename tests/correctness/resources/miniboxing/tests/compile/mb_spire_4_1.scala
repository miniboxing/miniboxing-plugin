package miniboxing.tests.compile.spire4.var1

import language.higherKinds

trait Dist[@miniboxed A] { self =>
  def sample[CC[A] <: Iterable[A]](n: Int)(implicit a: A): CC[A] =
    ???
}

// TreeCheckers complains about symbols even without miniboxing:
//
// $ cat x.scala 
// import language.higherKinds
//
// trait Dist[Z] { self =>
//   def sample[CC[A] <: Iterable[A]](n: Int)(implicit a: A): CC[Z] =
//     ???
// }
//
// $ scalac x.scala -Ycheck:typer
// x.scala:4: error: class A takes type parameters
//   def sample[CC[A] <: Iterable[A]](n: Int)(implicit a: A): CC[Z] =
//                                                        ^
// [Now checking: typer]
// [check: typer] The symbol, tpe or info of tree `(def sample[CC[A >: Nothing <: Any] >: [A]Nothing <: [A]Iterable[A]](n: Int)(implicit a: <error>): CC[Z] = scala.this.Predef.???) : [CC[A] <: Iterable[A]](n: Int)(implicit a: <error>)CC[Z]` refers to a out-of-scope symbol, type A. tree.symbol.ownerChain: method sample, trait Dist, package <empty>, package <root>
// warning: TreeCheckers detected non-compliant trees in x.scala
// one warning found
// one error found
 
