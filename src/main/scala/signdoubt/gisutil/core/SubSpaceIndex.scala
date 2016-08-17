package signdoubt.gisutil.core

import signdoubt.gisutil.store.SortedStore
import signdoubt.gisutil.{EasyConfig, Rectangle}

import scala.collection.{SortedSet, mutable}

/**
  * Index of [[SubSpace]] for each stored point.
  */
trait SubSpaceIndex extends SortedStore[ZValue, SubSpace] {
  val rootSpace = SubSpace(BitArray64("00"), -1)

  /**
    * Appends pointId as index entry.
    * This method is not executed in a transactional manner.
    * Only one thread should execute this method.
    *
    * @param pointId pointId to append
    */
  def append(pointId: ZValue): Unit

  /**
    * Removes pointId of index entry.
    * This method is not executed in a transactional manner.
    * The only one thread should execute this method.
    *
    * @param pointId pointId to remove
    */
  def remove(pointId: ZValue): Unit

  /**
    * Scans and returns [[SubSpace]]s within given region.
    *
    * @param rectangle Rectangle
    * @return [[SubSpace]]s
    */
  def scan(rectangle: Rectangle): Iterable[SubSpace]
}

/**
  * This class is sub-space for spatial query.
  *
  * @param spaceName      name (id) of [[SubSpace]]
  * @param estimatedCount estimated element count in this [[SubSpace]]
  */
case class SubSpace(spaceName: BitArray64, estimatedCount: Int) {
  private val one: Int = 1

  def zValue: ZValue = spaceName.longBits

  /**
    * [[ZValue]] range of this [[SubSpace]].
    *
    * @return
    */
  def zValueRange: (ZValue, ZValue) = (spaceName.longBits, spaceName.increment().longBits)

  /**
    * [[Rectangle]] of this [[SubSpace]] region.
    * The return value is (011, 01, 100, 10) if [[spaceName]] = 00111.
    *
    * @return [[SubSpace]] region
    */
  def rectangle: Rectangle = {
    val bits = spaceName.longBits
    val lower = bitwiseUnzip(bits)
    val sizeY: Int = spaceName.bitLength / 2
    val sizeX: Int = if (spaceName.bitLength % 2 == 0) sizeY else sizeY + 1
    val upper = (lower._1 + (one << (intBitSize - sizeX)), lower._2 + (one << (intBitSize - sizeY)))
    Rectangle.normalized(lower._1, lower._2, upper._1, upper._2)
  }

  /**
    * Increments estimated element count and returns updated [[SubSpace]].
    *
    * @return incremented [[SubSpace]]
    */
  def incrementEstimatedCount(): SubSpace = SubSpace(spaceName, estimatedCount + 1)

  /**
    * Decrements estimated element count and returns updated [[SubSpace]].
    *
    * @return decremented [[SubSpace]]
    */
  def decrementEstimatedCount(): SubSpace = SubSpace(spaceName, estimatedCount - 1)
}

/**
  * [[AsyncSubSpaceIndex]] may be updated asynchronously with data store.
  *
  * @param dataStore  data store, that stores values
  * @param indexStore index store
  * @param config     config
  */
class AsyncSubSpaceIndex(val dataStore: SortedStore[ZValue, _],
  val indexStore: SortedStore[ZValue, SubSpace],
  val config: EasyConfig = EasyConfig()) extends SubSpaceIndex {
  val pointCountMax = config.get(GisUtilConfig.pointCountMax)

  override def append(pointId: ZValue): Unit = {
    val targetSubSpace: SubSpace = indexStore.floorEntry(pointId).map(_._2).getOrElse(rootSpace)
    if (needSplit(targetSubSpace)) {
      rebuild(targetSubSpace)
    } else {
      indexStore.put(targetSubSpace.zValue, targetSubSpace.incrementEstimatedCount())
    }
  }

  /**
    * Rebuilds index of [[SubSpace]] with given range.
    *
    * @param targetSubSpace target [[SubSpace]]
    */
  def rebuild(targetSubSpace: SubSpace): Unit = {
    val (from, until) = targetSubSpace.zValueRange
    val pointIds = dataStore.range(from, until).keys
    val materialized = new MaterializedSubSpace(targetSubSpace.spaceName, SortedSet(pointIds.toSeq: _*))
    val childSpaces = splitFull(materialized)
    indexStore.put(childSpaces.map(ss => (ss.zValue, ss)))
  }

  private def splitFull(subSpace: MaterializedSubSpace): Seq[MaterializedSubSpace] = {
    // TODO enhance split, because split may need so many times
    val childSpaces = subSpace.split()
    val childQueue = mutable.Stack[MaterializedSubSpace]()
    childQueue.pushAll(childSpaces)
    val updateQueue = mutable.Queue[MaterializedSubSpace]()
    while (childQueue.nonEmpty) {
      val current = childQueue.pop()
      if (needSplit(current)) {
        childQueue.pushAll(current.split())
      } else {
        updateQueue.enqueue(current)
      }
    }
    updateQueue
  }

  private def needSplit(subSpace: SubSpace): Boolean = subSpace.estimatedCount > pointCountMax

  override def remove(pointId: ZValue): Unit = indexStore.floorEntry(pointId).foreach {
    entry =>
      val targetSubSpace = entry._2
      indexStore.put(targetSubSpace.zValue, targetSubSpace.decrementEstimatedCount())
  }

  override def scan(rectangle: Rectangle): Iterable[SubSpace] = {
    val request = ZValue(rectangle.lowerX, rectangle.lowerY)
    val until = ZValue(rectangle.upperX, rectangle.upperY)
    val floorSubSpace = indexStore.floorEntry(request).map(_._2)
    val candidateSubSpaces: Iterable[SubSpace] = floorSubSpace ++ indexStore.range(request, until).values
    candidateSubSpaces.filter(_.rectangle.overlap(rectangle))
  }

  override def from(from: ZValue, inclusive: Boolean): SortedStore[ZValue, SubSpace] =
    newIndex(indexStore.from(from, inclusive))

  private def newIndex(newStore: SortedStore[ZValue, SubSpace]): SubSpaceIndex =
    new AsyncSubSpaceIndex(dataStore, newStore, config)

  override def to(to: ZValue): SortedStore[ZValue, SubSpace] = newIndex(indexStore.to(to))

  override def until(to: ZValue): SortedStore[ZValue, SubSpace] = newIndex(indexStore.until(to))

  override def reverse(): SortedStore[ZValue, SubSpace] = newIndex(indexStore.reverse())

  override def toIterable: Iterable[(ZValue, SubSpace)] = indexStore.toIterable

  override def floorEntry(key: ZValue): Option[(ZValue, SubSpace)] = indexStore.floorEntry(key)

  override def ceilingEntry(key: ZValue): Option[(ZValue, SubSpace)] = indexStore.ceilingEntry(key)

  override def get(key: ZValue): Option[SubSpace] = indexStore.get(key)

  override def put(key: ZValue, value: SubSpace): Unit = indexStore.put(key, value)

  override def delete(key: ZValue): Unit = indexStore.delete(key)
}

/**
  * Materialized version of [[SubSpace]], which contains point-ids.
  *
  * @param spaceName [[SubSpace]] name
  * @param points    point-ids in this [[SubSpace]]
  */
class MaterializedSubSpace(spaceName: BitArray64, points: SortedSet[ZValue]) extends SubSpace(spaceName, points.size) {
  /**
    * Splits this [[SubSpace]]
    *
    * @return new [[SubSpace]]s
    */
  def split(): Array[MaterializedSubSpace] = {
    val childIds = (spaceName.appendZero, spaceName.appendOne)
    val boundary = childIds._2.longBits
    Array(
      new MaterializedSubSpace(childIds._1, points.until(boundary)),
      new MaterializedSubSpace(childIds._2, points.from(boundary)))
  }
}
