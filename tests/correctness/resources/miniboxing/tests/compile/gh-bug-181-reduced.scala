package miniboxing.tests.bug181.reduced
import annotation.tailrec

// Thanks to @julien-truffaut for spotting this bug in his `brique`
// project: https://github.com/julien-truffaut/brique

class ConsList[@miniboxed A] {
  def tail: ConsList[A] = ???
  @tailrec final def drop(): ConsList[A] = tail.drop()
}
