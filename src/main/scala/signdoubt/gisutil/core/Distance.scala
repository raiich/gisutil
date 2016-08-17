package signdoubt.gisutil.core

import signdoubt.gisutil.{Rectangle, RoundedPoint}

/**
  * Calculator of distance
  */
trait Distance {
  /**
    * Calculates distance to a point.
    *
    * @param other point
    * @return distance to a point
    */
  def to(other: RoundedPoint): Double

  /**
    * Calculates distance to a nearest point of rectangle
    *
    * @param rectangle rectangle
    * @return distance to a nearest point of rectangle
    */
  def toNearest(rectangle: Rectangle): Double

  /**
    * Calculates distance to a farthest point of rectangle
    *
    * @param rectangle rectangle
    * @return distance to a farthest point of rectangle
    */
  def toFarthest(rectangle: Rectangle): Double
}

/**
  * Calculator of euclidean distance
  *
  * @param base base point
  */
class EuclideanDistance(val base: RoundedPoint) extends Distance {
  override def to(other: RoundedPoint): Double = distance(base.x - other.x, base.y - other.y)

  private def distance(dx: Int, dy: Int): Double = Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2))

  override def toNearest(rectangle: Rectangle): Double = {
    if (rectangle.contains(base)) {
      0
    } else {
      val dx = if (rectangle.containsX(base.x)) 0
      else
        Math.min(Math.abs(rectangle.lowerX - base.x), Math.abs(rectangle.upperX - base.x))
      val dy = if (rectangle.containsY(base.y)) 0
      else
        Math.min(Math.abs(rectangle.lowerY - base.y), Math.abs(rectangle.upperY - base.y))
      distance(dx, dy)
    }
  }

  override def toFarthest(rectangle: Rectangle): Double = {
    val dx = Math.max(Math.abs(rectangle.lowerX - base.x), Math.abs(rectangle.upperX - base.x))
    val dy = Math.max(Math.abs(rectangle.lowerY - base.y), Math.abs(rectangle.upperY - base.y))
    distance(dx, dy)
  }
}

class DistanceHelper[V](distance: Distance) {
  def this(base: RoundedPoint) = this(new EuclideanDistance(base))

  def to(entry: (RoundedPoint, V)) = distance.to(entry._1)

  def toNearest(subSpace: SubSpace) = distance.toNearest(subSpace.rectangle)

  def toFarthest(subSpace: SubSpace) = distance.toFarthest(subSpace.rectangle)
}
