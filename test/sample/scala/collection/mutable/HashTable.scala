package sample.scala.collection.mutable

trait HashTable[A, Entry >: Null <: HashEntry[A, Entry]] extends HashTable.HashUtils[A] {
  import HashTable._

  protected var _loadFactor = defaultLoadFactor
  protected var table: Array[HashEntry[A, Entry]] = new Array(initialCapacity)
  protected var tableSize: Int = 0

  protected var threshold: Int = initialThreshold(_loadFactor)
  protected var sizemap: Array[Int] = null

  var seedvalue: Int = tableSizeSeed

  protected def tableSizeSeed = Integer.bitCount(table.length - 1)

  protected def initialSize: Int = HashTable.initialSize

  private def lastPopulatedIndex = {
    var idx = table.length - 1
    while (table(idx) == null && idx > 0)
      idx -= 1

    idx
  }

  protected def findEntry(key: A): Entry = {
    val h = index(elemHashCode(key))
    var e = table(h).asInstanceOf[Entry]
    while (e != null && !elemEquals(e.key, key)) e = e.next
    e
  }

  protected def addEntry(e: Entry) {
    val h = index(elemHashCode(e.key))
    e.next = table(h).asInstanceOf[Entry]
    table(h) = e
    tableSize = tableSize + 1
    nnSizeMapAdd(h)
    if (tableSize > threshold)
      resize(2 * table.length)
  }

  protected def removeEntry(key: A) : Entry = {
    val h = index(elemHashCode(key))
    var e = table(h).asInstanceOf[Entry]
    if (e != null) {
      if (elemEquals(e.key, key)) {
        table(h) = e.next
        tableSize = tableSize - 1
        nnSizeMapRemove(h)
        return e
      } else {
        var e1 = e.next
        while (e1 != null && !elemEquals(e1.key, key)) {
          e = e1
          e1 = e1.next
        }
        if (e1 != null) {
          e.next = e1.next
          tableSize = tableSize - 1
          nnSizeMapRemove(h)
          return e1
        }
      }
    }
    null
  }

  /** Avoid iterator for a 2x faster traversal. */
  protected final def foreachEntry[C](f: Entry => C) {
    val iterTable = table
    var idx       = lastPopulatedIndex
    var es        = iterTable(idx)

    while (es != null) {
      f(es.asInstanceOf[Entry])
      es = es.next

      while (es == null && idx > 0) {
        idx -= 1
        es = iterTable(idx)
      }
    }
  }

  /** Remove all entries from table
   */
  protected def clearTable() {
    var i = table.length - 1
    while (i >= 0) { table(i) = null; i = i - 1 }
    tableSize = 0
    nnSizeMapReset(0)
  }

  private def resize(newSize: Int) {
    val oldTable = table
    table = new Array(newSize)
    nnSizeMapReset(table.length)
    var i = oldTable.length - 1
    while (i >= 0) {
      var e = oldTable(i)
      while (e != null) {
        val h = index(elemHashCode(e.key))
        val e1 = e.next
        e.next = table(h).asInstanceOf[Entry]
        table(h) = e
        e = e1
        nnSizeMapAdd(h)
      }
      i = i - 1
    }
    threshold = newThreshold(_loadFactor, newSize)
  }

  protected def nnSizeMapAdd(h: Int) = if (sizemap ne null) {
    sizemap(h >> sizeMapBucketBitSize) += 1
  }

  protected def nnSizeMapRemove(h: Int) = if (sizemap ne null) {
    sizemap(h >> sizeMapBucketBitSize) -= 1
  }

  protected def nnSizeMapReset(tableLength: Int) = if (sizemap ne null) {
    val nsize = calcSizeMapSize(tableLength)
    if (sizemap.length != nsize) sizemap = new Array[Int](nsize)
    else java.util.Arrays.fill(sizemap, 0)
  }

  private[collection] final def totalSizeMapBuckets = if (sizeMapBucketSize < table.length) 1 else table.length / sizeMapBucketSize

  protected def calcSizeMapSize(tableLength: Int) = (tableLength >> sizeMapBucketBitSize) + 1

  // discards the previous sizemap and only allocates a new one
  protected def sizeMapInit(tableLength: Int) {
    sizemap = new Array[Int](calcSizeMapSize(tableLength))
  }

  // discards the previous sizemap and populates the new one
  protected def sizeMapInitAndRebuild() {
    sizeMapInit(table.length)

    // go through the buckets, count elements
    var tableidx = 0
    var bucketidx = 0
    val tbl = table
    var tableuntil = 0
    if (tbl.length < sizeMapBucketSize) tableuntil = tbl.length else tableuntil = sizeMapBucketSize
    val totalbuckets = totalSizeMapBuckets
    while (bucketidx < totalbuckets) {
      var currbucketsize = 0
      while (tableidx < tableuntil) {
        var e = tbl(tableidx)
        while (e ne null) {
          currbucketsize += 1
          e = e.next
        }
        tableidx += 1
      }
      sizemap(bucketidx) = currbucketsize
      tableuntil += sizeMapBucketSize
      bucketidx += 1
    }
  }

  private[collection] def printSizeMap() {
    println(sizemap.toList)
  }

  protected def sizeMapDisable() = sizemap = null

  protected def isSizeMapDefined = sizemap ne null

  // override to automatically initialize the size map
  protected def alwaysInitSizeMap = false

  /* End of size map handling code */

  protected def elemEquals(key1: A, key2: A): Boolean = (key1 == key2)

  // Note:
  // we take the most significant bits of the hashcode, not the lower ones
  // this is of crucial importance when populating the table in parallel
  protected final def index(hcode: Int) = {
    val ones = table.length - 1
    val improved = improve(hcode, seedvalue)
    val shifted = (improved >> (32 - java.lang.Integer.bitCount(ones))) & ones
    shifted
  }

  protected def initWithContents(c: HashTable.Contents[A, Entry]) = {
    if (c != null) {
      _loadFactor = c.loadFactor
      table = c.table
      tableSize = c.tableSize
      threshold = c.threshold
      seedvalue = c.seedvalue
      sizemap = c.sizemap
    }
    if (alwaysInitSizeMap && sizemap == null) sizeMapInitAndRebuild
  }

  private[collection] def hashTableContents = new HashTable.Contents(
    _loadFactor,
    table,
    tableSize,
    threshold,
    seedvalue,
    sizemap
  )
}

private[collection] object HashTable {
  /** The load factor for the hash table (in 0.001 step).
   */
  private[collection] final def defaultLoadFactor: Int = 750 // corresponds to 75%
  private[collection] final def loadFactorDenum = 1000;

  /** The initial size of the hash table.
   */
  private[collection] final def initialSize: Int = 16

  /** The initial threshold.
   */
  private[collection] final def initialThreshold(_loadFactor: Int): Int = newThreshold(_loadFactor, initialCapacity)

  private[collection] final def initialCapacity = capacity(initialSize)

  private[collection] final def newThreshold(_loadFactor: Int, size: Int) = ((size.toLong * _loadFactor) / loadFactorDenum).toInt

  private[collection] final def sizeForThreshold(_loadFactor: Int, thr: Int) = ((thr.toLong * loadFactorDenum) / _loadFactor).toInt

  private[collection] final def capacity(expectedSize: Int) = if (expectedSize == 0) 1 else powerOfTwo(expectedSize)

  trait HashUtils[KeyType] {
    protected final def sizeMapBucketBitSize = 5
    // so that:
    protected final def sizeMapBucketSize = 1 << sizeMapBucketBitSize

    protected def elemHashCode(key: KeyType) = key.##

    protected final def improve(hcode: Int, seed: Int) = {
      /* Murmur hash
       *  m = 0x5bd1e995
       *  r = 24
       *  note: h = seed = 0 in mmix
       *  mmix(h,k) = k *= m; k ^= k >> r; k *= m; h *= m; h ^= k; */
      // var k = hcode * 0x5bd1e995
      // k ^= k >> 24
      // k *= 0x5bd1e995
      // k

      /* Another fast multiplicative hash
       * by Phil Bagwell
       *
       * Comment:
       * Multiplication doesn't affect all the bits in the same way, so we want to
       * multiply twice, "once from each side".
       * It would be ideal to reverse all the bits after the first multiplication,
       * however, this is more costly. We therefore restrict ourselves only to
       * reversing the bytes before final multiplication. This yields a slightly
       * worse entropy in the lower 8 bits, but that can be improved by adding:
       *
       * `i ^= i >> 6`
       *
       * For performance reasons, we avoid this improvement.
       * */
      var i = hcode * 0x9e3775cd
      i = java.lang.Integer.reverseBytes(i)
      i = i * 0x9e3775cd
      // a slower alternative for byte reversal:
      // i = (i << 16) | (i >> 16)
      // i = ((i >> 8) & 0x00ff00ff) | ((i << 8) & 0xff00ff00)

      /* Jenkins hash
       * for range 0-10000, output has the msb set to zero */
      // var h = hcode + (hcode << 12)
      // h ^= (h >> 22)
      // h += (h << 4)
      // h ^= (h >> 9)
      // h += (h << 10)
      // h ^= (h >> 2)
      // h += (h << 7)
      // h ^= (h >> 12)
      // h

      /* OLD VERSION
       * quick, but bad for sequence 0-10000 - little enthropy in higher bits
       * since 2003 */
      // var h: Int = hcode + ~(hcode << 9)
      // h = h ^ (h >>> 14)
      // h = h + (h << 4)
      // h ^ (h >>> 10)

      // the rest of the computation is due to SI-5293
      val rotation = seed % 32
      val rotated = (i >>> rotation) | (i << (32 - rotation))
      rotated
    }
  }

  /**
   * Returns a power of two >= `target`.
   */
  private[collection] def powerOfTwo(target: Int): Int = {
    /* See http://bits.stephan-brumme.com/roundUpToNextPowerOfTwo.html */
    var c = target - 1;
    c |= c >>>  1;
    c |= c >>>  2;
    c |= c >>>  4;
    c |= c >>>  8;
    c |= c >>> 16;
    c + 1;
  }

  class Contents[A, Entry >: Null <: HashEntry[A, Entry]](
    val loadFactor: Int,
    val table: Array[HashEntry[A, Entry]],
    val tableSize: Int,
    val threshold: Int,
    val seedvalue: Int,
    val sizemap: Array[Int]
  ) {
  }

}
