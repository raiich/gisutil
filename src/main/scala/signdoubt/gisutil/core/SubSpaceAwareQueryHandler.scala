package signdoubt.gisutil.core

import signdoubt.gisutil.store.SortedStore
import signdoubt.gisutil.{EasyConfig, Rectangle, RoundedPoint, SpatialQueryHandler}

/**
  * [[SpatialQueryHandler]] using [[SubSpace]].
  *
  * @param dataStore data store
  * @param index     index
  * @param config    config
  * @tparam V value type for each point key
  */
class SubSpaceAwareQueryHandler[V](dataStore: SortedStore[ZValue, V], index: SubSpaceIndex,
  config: EasyConfig = EasyConfig()) extends SpatialQueryHandler[V] {

  override def put(point: RoundedPoint, value: V): Unit = {
    val pointId = ZValue(point)
    dataStore.put(pointId, value)
    index.append(pointId)
  }

  override def get(point: RoundedPoint): Option[V] = dataStore.get(ZValue(point))

  override def delete(point: RoundedPoint): Unit = {
    val key = ZValue(point)
    dataStore.delete(key)
    index.remove(key)
  }

  override def scan(rectangle: Rectangle): Iterable[(RoundedPoint, V)] = {
    val subSpaces = index.scan(rectangle)
    val entries = subSpaces.map(ss => scanPointsInSubSpace(ss).filter(_._1.overlap(rectangle)))
    entries.flatten
  }

  private def scanPointsInSubSpace(subSpace: SubSpace): Iterable[(RoundedPoint, V)] = {
    val (from, to) = subSpace.zValueRange
    val entries = dataStore.range(from, to).toIterable
    val restored = entries.map(entry => (RoundedPoint(bitwiseUnzip(entry._1)), entry._2))
    restored
  }
}
