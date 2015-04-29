package mbhashmap
import language.higherKinds

trait Buildable[@miniboxed T, Container[_]] extends Iterable[T] {
  def builder[@miniboxed K]: Builder[K, Container]

  def map[@miniboxed U](f: T => U) = {
    val bd = builder[U]
    val it = iterator
    while (it.hasNext) bd.append(f(it.next))
    bd.finalise
  }

  def filter(f: T => Boolean) = {
    val bd = builder[T]
    val it = iterator
    while (it.hasNext) {
      val elem = it.next
      if (f(elem)) {
        bd.append(elem)
      }
    }
    bd.finalise
  }
}

trait Buildable2[@miniboxed A, @miniboxed B, Container[_, _]] extends Iterable[(A, B)] {
  def builder[@miniboxed U, @miniboxed V]: Builder2[U, V, Container]
  
  def map[@miniboxed U, @miniboxed V](f: ((A, B)) => (U, V)) = {
    val bd = builder[U, V]
    val it = iterator
    while (it.hasNext) bd.append(f(it.next))
    bd.finalise
  }

  def filter(f: ((A, B)) => Boolean) = {
    val bd = builder[A, B]
    val it = iterator
    while (it.hasNext) {
      val elem = it.next
      if (f(elem)) {
        bd.append(elem)
      }
    }
    bd.finalise
  }
}

abstract class Builder[@miniboxed T, Container[_]] {
  def append(x: T)
  def finalise: Container[T]
}

abstract class Builder2[@miniboxed A, @miniboxed B, Container[_, _]] {
  def append(x: (A, B))
  def finalise: Container[A, B]
}

trait Iterable[@miniboxed T] {
  def iterator: Iterator[T]

  def foreach(f: (T) => Unit) = {
    val it = iterator
    while (it.hasNext) {
      f(it.next)
    }
  }
}

abstract class Iterator[@miniboxed T] {
  def next: T
  def hasNext: Boolean
}


object MbHashmap {
  def empty[@miniboxed K, @miniboxed V](initialCapacity: Int = 16, loadFactor: Float = 0.75f) =
    new MbHashmap[K, V](initialCapacity, loadFactor)

  class MbHashmap[@miniboxed K, @miniboxed V](private var _capacity: Int, private val _loadFactor: Float)
    extends Buildable2[K, V, MbHashmap] {
    
    private var _buckets = new Array[Entry[K, V]](_capacity)
    private var _size = 0

    def size = _size

    def apply(key: K): Option[V] = {
      var entry = _buckets(computeIndex(key))

      while (entry != null) {
        if (entry.key == key) {
          return Some(entry.value)
        }
        entry = entry.next
      }

      None
    }

    def update(key: K, value: V): Unit = {
      if (updateEntry(key, value)) {
        _size += 1
        rehashIfNeeded
      }
    }

    private def updateEntry(key: K, value: V): Boolean = {
      val i = computeIndex(key)
      var entry = _buckets(i)

      if (entry == null) {
        _buckets(i) = new Entry(key, value)
      } else {
        var lastEntry = entry

        while (entry != null) {
          if (entry.key == key) {
            entry.value = value
            return false
          }
          lastEntry = entry
          entry = entry.next
        }

        lastEntry.next = new Entry(key, value)
      }
      true
    }

    private def rehashIfNeeded = {
      if (_size > _capacity * _loadFactor) {
        val oldCapacity = _capacity
        val oldBuckets = _buckets

        _capacity = _capacity * 2
        _buckets = new Array[Entry[K, V]](_capacity)

        var i = 0
        while (i < oldCapacity) {
          var entry = oldBuckets(i)
          while (entry != null) {
            updateEntry(entry.key, entry.value)
            entry = entry.next
          }
          i += 1
        }

        /*
        println("Rehashing")
        println("\tSize : " + _size)
        println("\tOld capacity : " + oldCapacity)
        println("\tNew capacity : " + _capacity)
        */
      }
    }

    private def computeIndex(k: K) = k.hashCode() % _capacity
    
    def iterator = new Iterator[(K, V)] {
      private var _i = 0
      private var _next: Entry[K, V] = null
      computeNext
      
      def hasNext = _next != null
      
      def next = {
        val n = _next
        computeNext
        (n.key, n.value)
      }
      
      private def computeNext: Unit = {
        while (_i < _capacity && _buckets(_i) == null) {
          _i += 1
        }
        
        if (_i >= _capacity) {
          _next = null
        } else if (_next == null) {
          _next = _buckets(_i)
        } else {
          if (_next.next == null) {
            _i += 1
            _next = null
            computeNext
          } else {
            _next = _next.next
          }
        }
      }
    }
    
    def builder[@miniboxed U, @miniboxed V] = new Builder2[U, V, MbHashmap] {
      private var _hm = MbHashmap.empty[U, V]()
      
      def append(x: (U, V)) = {
        _hm(x._1) = x._2
      }
      
      def finalise = _hm
    }
  }

  class Entry[@miniboxed K, @miniboxed V](val key: K, var value: V) {
    var next: Entry[K, V] = null
  }
}

object Main {
  def main(args: Array[String]) = {
    val hm = MbHashmap.empty[Int, Int]()
    for (i <- 0 to 50) {
      hm(i) = i
    }
    for (i <- 0 to 50) {
      hm(i).foreach { x => assert(x == i) }
    }
    
    hm.map(pair => (pair._1, pair._2 * 2)).foreach(pair => { println(pair._1 + " => " + pair._2)})
  }
}
