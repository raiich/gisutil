package signdoubt.gisutil.store

import scala.collection.mutable

class AccessLoggingSortedStore[K, V](
    store: SortedStore[K, V], val logger: StoreLogger[K, V]) extends SortedStore[K, V] {
  def this() = this(new InMemorySortedStore[K, V], new StoreLogger[K, V]())

  def newStore(store: SortedStore[K, V]) = new AccessLoggingSortedStore(store, logger)

  override def from(from: K, inclusive: Boolean): SortedStore[K, V] = newStore(store.from(from, inclusive))

  override def to(to: K): SortedStore[K, V] = newStore(store.to(to))

  override def until(until: K): SortedStore[K, V] = newStore(store.until(until))

  override def reverse(): SortedStore[K, V] = newStore(store.reverse())

  override def floorEntry(key: K): Option[(K, V)] = {
    val floorEntry = store.floorEntry(key)
    logger.put(key, floorEntry.map(_._2))
    floorEntry
  }

  override def ceilingEntry(key: K): Option[(K, V)] = {
    val ceilingEntry = store.ceilingEntry(key)
    logger.put(key, ceilingEntry.map(_._2))
    ceilingEntry
  }

  override def get(key: K): Option[V] = {
    val value = store.get(key)
    logger.put(key, value)
    value
  }

  override def put(key: K, value: V): Unit = {
    logger.put(key, Option(value))
    store.put(key, value)
  }

  override def delete(key: K): Unit = {
    logger.put(key, None)
    store.delete(key)
  }

  override def toIterable: Iterable[(K, V)] = {
    new Iterable[(K, V)] {
      override def iterator: Iterator[(K, V)] = new AccessLoggingIterator[K, V](store.toIterable.iterator, logger)
    }
  }
}

class AccessLoggingIterator[K, V](
    iterator: Iterator[(K, V)], logger: StoreLogger[K, V]) extends Iterator[(K, V)] {
  override def hasNext: Boolean = iterator.hasNext

  override def next(): (K, V) = {
    val next = iterator.next()
    logger.put(next._1, Option(next._2))
    next
  }
}

class StoreLogger[K, V] {
  var buffer = mutable.Buffer[(K, Option[V])]()

  def put(key: K, value: Option[V]): Unit = put((key, value))

  def put(entry: (K, Option[V])) = buffer += entry

  def iterator = buffer.iterator

  def clear() = buffer.clear()
}
