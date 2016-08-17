package signdoubt.gisutil.store

import scala.collection.concurrent.TrieMap

class InMemoryKeyValueStore[K, V](val map: scala.collection.mutable.Map[K, V] = TrieMap[K, V]())
  extends KeyValueStore[K, V] {
  override def get(key: K): Option[V] = map.get(key)

  override def put(key: K, value: V) = map.put(key, value)

  override def delete(key: K) = map.remove(key)

  override def toIterable: Iterable[(K, V)] = map.map(t => (t._1, t._2)).seq
}
