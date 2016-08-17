package signdoubt.gisutil

/**
  * This class is able to handle spatial query, like range scan, k-nearest neighbor.
  *
  * @tparam V value type for each point key
  */
trait SpatialQueryHandler[V] {
  /**
    * Adds a new point/value relation to this handler.
    * If the map already contains a mapping for the point, it will be overridden by the new value.
    *
    * @param point the point to update
    * @param value the new value
    */
  def put(point: RoundedPoint, value: V): Unit

  /**
    * Optionally returns the value associated with a point.
    *
    * @param point point
    * @return an option value containing the value associated with `point` in this handler,
    *         or `None` if none exists.
    */
  def get(point: RoundedPoint): Option[V]

  /**
    * Scans and returns point/value relations within given range.
    *
    * @param rectangle range
    * @return point/value entries within given range
    */
  def scan(rectangle: Rectangle): Iterable[(RoundedPoint, V)]

  /**
    * Deletes a point and associated value from this handler.
    *
    * @param point a point to be deleted
    */
  def delete(point: RoundedPoint): Unit

  /**
    * Search and returns k-nearest neighbors.
    * @param base base point
    * @param k search count of neighbors
    * @return k-nearest neighbors
    */
  def nearestNeighbor(base: RoundedPoint, k: Int): Iterable[(RoundedPoint, V)] = throw new NotImplementedError()
}

/**
  * This represents a point value, which is rounded to Int type.
  *
  * @param x x value
  * @param y y value
  */
case class RoundedPoint(x: Int, y: Int) {
  /**
    * Checks if the [[Rectangle]] and this overlaps.
    *
    * @param rectangle rectangle
    * @return true if this and rectangle overlaps
    */
  def overlap(rectangle: Rectangle): Boolean = rectangle.contains(this)
}

object RoundedPoint {
  def apply(tuple: (Int, Int)): RoundedPoint = new RoundedPoint(tuple._1, tuple._2)
}

case class Rectangle(lowerX: Int, lowerY: Int, upperX: Int, upperY: Int) {
  def contains(point: RoundedPoint): Boolean = contains(point.x, point.y)

  def contains(tuple: (Int, Int)): Boolean = contains(tuple._1, tuple._2)

  /**
    * Checks if the given point (x, y) is contained by this [[Rectangle]].
    *
    * @param x x of a given point
    * @param y y of a given point
    * @return true if this [[Rectangle]] contains given point
    */
  def contains(x: Int, y: Int): Boolean = containsX(x) && containsY(y)

  def containsX(x: Int): Boolean = between(lowerX, x, upperX)

  private def between(lower: Int, v: Int, upper: Int) =
    lower <= v && (v < upper || upper < 0 || upper == Int.MaxValue && v == Int.MaxValue)

  def containsY(y: Int): Boolean = between(lowerY, y, upperY)

  /**
    * Checks if other and this [[Rectangle]] overlap each other.
    *
    * @param other rectangle
    * @return true if other and this [[Rectangle]] overlap
    */
  def overlap(other: Rectangle): Boolean =
    (containsX(other.lowerX) || other.containsX(lowerX)) && (containsY(other.lowerY) || other.containsY(lowerY))
}

object Rectangle {
  def apply(lower: RoundedPoint, upper: RoundedPoint): Rectangle = normalized(lower.x, lower.y, upper.x, upper.y)

  /**
    * Normalizes given bounds and returns [[Rectangle]].
    * `Normalize` means each bound is converted to 0 <= bound <= [[Int.MaxValue]].
    *
    * @param lowerX lower bound of x
    * @param lowerY lower bound of y
    * @param upperX upper bound of x
    * @param upperY upper bound of y
    * @return rectangle with normalized bounds
    */
  def normalized(lowerX: Int, lowerY: Int, upperX: Int, upperY: Int): Rectangle =
    Rectangle(fix(lowerX, 0), fix(lowerY, 0), fix(upperX, Int.MaxValue), fix(upperY, Int.MaxValue))

  private def fix(v: Int, alt: Int): Int = if (v < 0) alt else v

  def apply(base: RoundedPoint, width: Int): Rectangle =
    normalized(base.x - width, base.y - width, base.x + width, base.y + width)
}
