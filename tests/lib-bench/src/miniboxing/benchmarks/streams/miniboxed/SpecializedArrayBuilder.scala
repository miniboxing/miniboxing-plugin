package miniboxing.benchmarks.streams.miniboxed

import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder
import scala.Array
import scala.collection.mutable.ArrayBuffer

trait SpecializedArrayBuilder[@specialized(Long) A] {
  def +=(a: A)
  def result: Array[A]
}

trait LowPrioritySpecializedArrayBuilder {
  implicit def defaultBuilder[A: ClassTag] = new SpecializedArrayBuilder[A] {
    val builder = ArrayBuilder.make[A]
    def +=(a: A) = builder += a
    def result = builder.result
  }
}

object SpecializedArrayBuilder extends LowPrioritySpecializedArrayBuilder {
  implicit val longBuilder = new SpecializedArrayBuilder[Long] {
    val builder = new ArrayBuilder.ofLong
    def +=(a: Long) = builder += a
    def result = builder.result
  }
}
