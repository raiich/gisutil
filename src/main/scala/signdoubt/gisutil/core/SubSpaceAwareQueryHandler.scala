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

  override def nearestNeighbor(base: RoundedPoint, k: Int): Iterable[(RoundedPoint, V)] = {
    val distance = new DistanceHelper[V](base)
    var resultQueue = new SortedPriorityQueue[(RoundedPoint, V)](Ordering.by(distance.to).reverse)
    val spaceQueue = new SubSpaceRetriever(
      index.from(ZValue(base)).toIterable.toIterator,
      index.until(ZValue(base)).reverse().toIterable.toIterator,
      distance)
    var width = 0
    var kthDistance: Double = Double.MaxValue

    while (true) {
      // expand search region
      if (spaceQueue.isEmpty) {
        val expanded = spaceQueue.expandSearchRegionAndEnqueue(Rectangle(base, width))
        if (!expanded) {
          // all scanned
          return resultQueue.dequeueAll
        }
      }
      // next nearest subspace
      val space = spaceQueue.dequeue()
      // no more nearest subspaces within k-th distance
      if (kthDistance <= distance.toNearest(space)) {
        return resultQueue.dequeueAll
      }
      // enqueue order by distance from base
      scanPointsInSubSpace(space).foreach(tuple => resultQueue.enqueue(tuple))
      if (resultQueue.size >= k) {
        // drop more than k-th elements
        resultQueue = resultQueue.fit(k)
        // update k-th distance
        kthDistance = distance.to(resultQueue.last)
      }
      // update width
      val mayNextWidth: Int = Math.ceil(distance.toFarthest(space)).asInstanceOf[Int]
      width = Math.max(width, mayNextWidth)
    }
    resultQueue.dequeueAll
  }

  private def scanPointsInSubSpace(subSpace: SubSpace): Iterable[(RoundedPoint, V)] = {
    val (from, to) = subSpace.zValueRange
    val entries = dataStore.range(from, to).toIterable
    val restored = entries.map(entry => (RoundedPoint(bitwiseUnzip(entry._1)), entry._2))
    restored
  }
}

class SubSpaceRetriever(
  forward: Iterator[(ZValue, SubSpace)], backward: Iterator[(ZValue, SubSpace)],
  distance: DistanceHelper[_]) {
  val fetchedQueue = new SortedPriorityQueue[SubSpace](Ordering.by(distance.toNearest).reverse)
  var region: Rectangle = _

  def expandSearchRegionAndEnqueue(nextRegion: Rectangle): Boolean = {
    region = nextRegion
    val floor = ZValue(region.lowerX, region.lowerY)
    val ceil = ZValue(region.upperX, region.upperY)
    enqueueWithPredicate(forward, _ <= ceil)
    enqueueWithPredicate(backward, _ >= floor)
    nonEmpty
  }

  private def enqueueWithPredicate(iterator: Iterator[(ZValue, SubSpace)], predicate: ZValue => Boolean) {
    while (iterator.hasNext) {
      val (zid, entry) = iterator.next()
      fetchedQueue.enqueue(entry)
      if (!predicate(zid)) {
        return
      }
    }
  }

  def nonEmpty: Boolean = fetchedQueue.nonEmpty && fetchedQueue.head.rectangle.overlap(region)

  def isEmpty: Boolean = !nonEmpty

  def dequeue(): SubSpace = fetchedQueue.dequeue()
}
