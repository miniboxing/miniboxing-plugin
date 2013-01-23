package miniboxing.sample.scala.collection.mutable

class HashMap[A, B] private[collection] (contents: HashTable.Contents[A, DefaultEntry[A, B]])
   extends HashTable[A, DefaultEntry[A, B]]
{
  type Entry = DefaultEntry[A, B]

  def empty: HashMap[A, B] = HashMap.empty[A, B]
  def clear() = clearTable()
  def size: Int = tableSize

  def this() = this(null)


  // contains and apply overridden to avoid option allocations.
  def contains(key: A) = findEntry(key) != null
  def apply(key: A): B = {
    val result = findEntry(key)
    if (result == null) throw new Exception("key not found")
    else result.value
  }

  def get(key: A, default : B): B = {
    val e = findEntry(key)
    if (e == null) default
    else e.value
  }

  def getOrElseUpdate(key: A, op: => B): B = {
    val upd = op
    val old = get(key, upd)
    if (upd != old) put(key, upd)
    upd
  }

  def put(key: A, value: B): Unit = {
    val e = findEntry(key)
    if (e == null) addEntry(new Entry(key, value))
    else { val v = e.value; e.value = value }
  }

  def update(key: A, value: B): Unit = put(key, value)

  def remove(key: A): Unit = {
    removeEntry(key)
  }

  def += (kv: (A, B)): this.type = {
    val e = findEntry(kv._1)
    if (e == null) addEntry(new Entry(kv._1, kv._2))
    else e.value = kv._2
    this
  }

  def -=(key: A): this.type = { removeEntry(key); this }

  def foreach[C](f: ((A, B)) => C): Unit = foreachEntry(new FunctionOnEntry(f))

  /** Toggles whether a size map is used to track hash map statistics.
   */
  def useSizeMap(t: Boolean) = if (t) {
    if (!isSizeMapDefined) sizeMapInitAndRebuild
  } else sizeMapDisable
}

class FunctionOnEntry[A, B, C](f : ((A, B)) => C) extends Function1[DefaultEntry[A, B], C]{
  override def apply(e: DefaultEntry[A, B]) : C = f(e.key, e.value)
}

object HashMap {
  def empty[A, B]: HashMap[A, B] = EmptyHashMap.asInstanceOf[HashMap[A, B]]
  private object EmptyHashMap extends HashMap[Any, Nothing] { }
}
