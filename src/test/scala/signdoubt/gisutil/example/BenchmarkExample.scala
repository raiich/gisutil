package signdoubt.gisutil.example

import signdoubt.gisutil.core.SpatialQueryHandlerFactory
import signdoubt.gisutil.{Rectangle, RoundedPoint}

object BenchmarkExample {
  val lowerX = 350000000
  val lowerY = 1350000000
  val upperX = 380000000
  val upperY = 1380000000

  def main(args: Array[String]): Unit = {
    val ps = Array(
      createPoints(20),
      createPoints(80),
      createPoints(160)
    )
    val rs = Array(
      createRange(20),
      createRange(80),
      createRange(160)
    )

    for (ranges <- rs; points <- ps) {
      println("point size: " + points.size + ", range size: " + ranges.size)
      test(points, ranges)
    }
  }

  def createPoints(size: Int): Seq[RoundedPoint] = {
    val strideX = (upperX - lowerX) / size
    val strideY = (upperY - lowerY) / size
    val points = for (i <- 0 until size; j <- 0 until size)
      yield RoundedPoint(lowerX + i * strideX, lowerY + j * strideY)

    points
  }

  def createRange(size: Int): Seq[Rectangle] = {
    val strideX = (upperX - lowerX) / size
    val strideY = (upperY - lowerY) / size
    val ranges = for (i <- 0 until size; j <- 0 until size)
      yield Rectangle(lowerX + i * strideX, lowerY + j * strideY,
        lowerX + (i + 1) * strideX, lowerY + (j + 1) * strideY)
    ranges
  }

  def test(points: Seq[RoundedPoint], ranges: Seq[Rectangle]) {

    val handlers = Array(
      SpatialQueryHandlerFactory.subSpaceAwareQueryHandlerInMemory[String](),
      SpatialQueryHandlerFactory.naiveQueryHandlerInMemory[String]()
    )

    handlers.foreach { handler =>
      println(handler.getClass)
      val long = System.nanoTime()
      points.foreach { point => handler.put(point, point.toString) }
      val end = System.nanoTime()
      println("  put: " + (end - long))
    }

    handlers.foreach { handler =>
      println(handler.getClass)
      val long = System.nanoTime()
      ranges.foreach { range => handler.scan(range) }
      val end = System.nanoTime()
      println("  get: " + (end - long))
    }
  }
}
