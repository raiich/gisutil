package signdoubt.gisutil.core

import signdoubt.gisutil._
import signdoubt.gisutil.store.{InMemoryKeyValueStore, InMemorySortedStore, KeyValueStore, SortedStore}

object SpatialQueryHandlerFactory {
  /**
    * [[SubSpace]] aware search implementation of [[SpatialQueryHandler]] (in-memory version).
    *
    * @param config config
    * @tparam V value type
    * @return [[SubSpace]] aware search implementation of [[SpatialQueryHandler]] (in-memory version)
    */
  def subSpaceAwareQueryHandlerInMemory[V](config: EasyConfig = EasyConfig()): SpatialQueryHandler[V] =
    subSpaceAwareQueryHandler(new InMemorySortedStore[ZValue, V])

  /**
    * [[SubSpace]] aware search implementation of [[SpatialQueryHandler]].
    *
    * @param dataStore  data store
    * @param indexStore index store
    * @param config     config
    * @tparam V value type
    * @return [[SubSpace]] aware search implementation of [[SpatialQueryHandler]]
    */
  def subSpaceAwareQueryHandler[V](
      dataStore: SortedStore[ZValue, V],
      indexStore: SortedStore[ZValue, SubSpace] = new InMemorySortedStore[ZValue, SubSpace],
      config: EasyConfig = EasyConfig()): SpatialQueryHandler[V] =
    new SubSpaceAwareQueryHandler[V](dataStore, new AsyncSubSpaceIndex(dataStore, indexStore, config), config)

  /**
    * Naive search implementation of [[SpatialQueryHandler]] (in-memory version).
    *
    * @tparam V value type
    * @return Naive search implementation of [[SpatialQueryHandler]] (in-memory version)
    */
  def naiveQueryHandlerInMemory[V](): SpatialQueryHandler[V] =
    naiveQueryHandler(new InMemoryKeyValueStore[RoundedPoint, V])

  /**
    * Naive search implementation of [[SpatialQueryHandler]].
    *
    * @param store store for data
    * @tparam V value type
    * @return Naive search implementation of [[SpatialQueryHandler]]
    */
  def naiveQueryHandler[V](store: KeyValueStore[RoundedPoint, V]): SpatialQueryHandler[V] =
    new NaiveFullScanQueryHandler[V](store)

  def naiveQueryHandlerInMemoryZOrder[V](): SpatialQueryHandler[V] =
    new NaiveFullScanQueryHandler[V](
      new ZOrderKeyValueStore[V](
        new InMemorySortedStore[ZValue, V]()))

}

private class ZOrderKeyValueStore[V](sortedStore: SortedStore[ZValue, V]) extends KeyValueStore[RoundedPoint, V] {
  override def get(key: RoundedPoint): Option[V] = sortedStore.get(ZValue(key))

  override def put(key: RoundedPoint, value: V): Unit = sortedStore.put(ZValue(key), value)

  override def delete(key: RoundedPoint): Unit = sortedStore.delete(ZValue(key))

  override def toIterable: Iterable[(RoundedPoint, V)] =
    sortedStore.toIterable.map(entry => (RoundedPoint(bitwiseUnzip(entry._1)), entry._2))
}

private class NaiveFullScanQueryHandler[V](val store: KeyValueStore[RoundedPoint, V]) extends SpatialQueryHandler[V] {
  override def put(point: RoundedPoint, value: V): Unit = store.put(point, value)

  override def get(point: RoundedPoint): Option[V] = store.get(point)

  override def scan(rectangle: Rectangle): Iterable[(RoundedPoint, V)] =
    store.toIterable.filter(_._1.overlap(rectangle))

  override def delete(point: RoundedPoint): Unit = store.delete(point)
}
