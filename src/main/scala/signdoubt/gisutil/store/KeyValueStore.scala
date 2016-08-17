package signdoubt.gisutil.store

/**
  * Key-Value Store interface.
  *
  * @tparam K key type
  * @tparam V value type
  */
trait KeyValueStore[K, V] {
  /** Optionally returns the value associated with a key.
    *
    * @param  key the key value
    * @return an option value containing the value associated with `key` in this store,
    *         or `None` if none exists.
    */
  def get(key: K): Option[V]

  /**
    * Adds a new key/value pair to this store.
    * If the map already contains a
    * mapping for the key, it will be overridden by the new value.
    *
    * @param key   the key to update
    * @param value the new value
    */
  def put(key: K, value: V): Unit

  /**
    * Puts multiple entries.
    * But default implementation is not in transactional (atomic) manner.
    * This method needs to be implemented properly.
    *
    * @param entries entries to put
    */
  def put(entries: Seq[(K, V)]): Unit = entries.foreach(t => put(t._1, t._2))

  /**
    * Deletes a key from this store.
    *
    * @param key the key to be deleted
    */
  def delete(key: K): Unit

  /**
    * Returns all keys in this store.
    *
    * @return keys
    */
  def keys: Iterable[K] = toIterable.map(_._1)

  /**
    * Returns key/value entries in this store.
    *
    * @return entries
    */
  def toIterable: Iterable[(K, V)]

  /**
    * Returns values in this store.
    *
    * @return values
    */
  def values: Seq[V] = toIterable.map(_._2).toSeq

  /**
    * Syncs with underling store.
    * The default implementation throws [[UnsupportedOperationException]].
    */
  def sync() = throw new UnsupportedOperationException("no need to sync")
}
