package miniboxing.simple



class Pair[@miniboxed T1,@miniboxed T2](t1: T1, t2: T2) {
  def defT1 : T1 = t1
  def defT2 : T2 = t2
  var varT1 : T1 = t1
}


//class Pair_LL[T1, T2](t1: T1, t2: T2) {
//  def a : T1 = t1
//  def b : T2 = t2
//  var c : T1 = t1
//}
//
//
//
//
