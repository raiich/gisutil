package signdoubt.gisutil.store

import java.util.Comparator
import java.util.concurrent.ConcurrentSkipListMap

import scala.collection.JavaConversions._

class InMemorySortedStore[K, V](val map: java.util.NavigableMap[K, V] = new ConcurrentSkipListMap[K, V])
  extends SortedStore[K, V] {
  def this(comparator: Comparator[K]) = this(new ConcurrentSkipListMap[K, V](comparator: Comparator[K]))

  override def from(from: K) = new InMemorySortedStore(map.tailMap(from, true))

  override def from(from: K, inclusive: Boolean) = new InMemorySortedStore(map.tailMap(from, inclusive))

  override def to(to: K) = new InMemorySortedStore(map.headMap(to, true))

  override def until(to: K) = new InMemorySortedStore(map.headMap(to, false))

  override def range(from: K, until: K): SortedStore[K, V] =
    new InMemorySortedStore[K, V](map.subMap(from, true, until, false))

  override def reverse() = new InMemorySortedStore(map.descendingMap())

  override def floorEntry(key: K): Option[(K, V)] = Option(map.floorEntry(key)).map(tuple)

  override def ceilingEntry(key: K): Option[(K, V)] = Option(map.higherEntry(key)).map(tuple)

  def tuple(entry: java.util.Map.Entry[K, V]): (K, V) = (entry.getKey, entry.getValue)

  override def get(key: K): Option[V] = Option(map.get(key))

  override def put(key: K, value: V) = map.put(key, value)

  override def delete(key: K) = Option(map.remove(key))

  override def keys: Iterable[K] = map.keySet()

  override def values: Seq[V] = map.values().toSeq

  override def toIterable: Iterable[(K, V)] = map.toIterable
}
