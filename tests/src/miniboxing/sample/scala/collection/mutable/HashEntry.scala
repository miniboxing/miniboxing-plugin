package miniboxing.sample.scala.collection.mutable

trait HashEntry [A, E] {
  val key: A
  var next: E = _
}
