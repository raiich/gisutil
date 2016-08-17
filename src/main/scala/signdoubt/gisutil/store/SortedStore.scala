package signdoubt.gisutil.store

/**
  * Each entry of this store is sorted by key order.
  * This represents like wide column store interface.
  *
  * @tparam K key type
  * @tparam V value type
  */
trait SortedStore[K, V] extends KeyValueStore[K, V] {
  val inclusive: Boolean = true

  /**
    * Returns sub store view, which starts with given key inclusive.
    *
    * @param from start key (inclusive)
    * @return sub store view
    */
  def from(from: K): SortedStore[K, V] = this.from(from, inclusive)

  /**
    * Returns sub store view, which starts with given key.
    *
    * @param from      start key
    * @param inclusive inclusive
    * @return sub store view
    */
  def from(from: K, inclusive: Boolean): SortedStore[K, V]

  /**
    * Returns sub store view, up to given key inclusive.
    *
    * @param to end key (inclusive)
    * @return sub store view
    */
  def to(to: K): SortedStore[K, V]

  /**
    * Returns sub store view, up to given key, exclusive.
    *
    * @param until end key (exclusive)
    * @return sub store view
    */
  def until(until: K): SortedStore[K, V]

  /**
    * Returns sub store view, with given start and end key.
    *
    * @param from  start key (inclusive)
    * @param until end key (exclusive)
    * @return sub store view
    */
  def range(from: K, until: K): SortedStore[K, V] = this.from(from).until(until)

  /**
    * Returns reverse key-order store view.
    *
    * @return reverse key-order store view
    */
  def reverse(): SortedStore[K, V]

  /**
    * Finds and returns floor entry of given key in this store.
    *
    * @param key find key
    * @return floor entry
    */
  def floorEntry(key: K): Option[(K, V)]

  /**
    * Finds and returns ceil entry of given key in this store.
    *
    * @param key find key
    * @return ceil entry
    */
  def ceilingEntry(key: K): Option[(K, V)]
}
