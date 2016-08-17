package signdoubt.gisutil.core

import org.scalatest.FunSuite
import signdoubt.gisutil.{Rectangle, RoundedPoint, SpatialQueryHandler}

abstract class QueryHandlerSuite extends FunSuite {
  def fixture: SpatialQueryHandler[String]

  def testNearest(iterable: Iterable[(RoundedPoint, String)], expected: (Int, Int)*) {
    assert(iterable.toSeq == expected.map(tuple => (point(tuple), tuple.toString)))
  }

  def testNearestSet(iterable: Iterable[(RoundedPoint, String)], expected: (Int, Int)*) {
    assert(Set(iterable.toSeq: _*) == Set(expected.toSeq.map(tuple => (point(tuple), tuple.toString)): _*))
  }

  test("basic test") {
    val handler = fixture
    assert(handler.get(point(0, 0)).isEmpty)
    handler.put(point(1, 1), "(1, 1)")
    assert(handler.get(point(1, 1)).get == "(1, 1)")
    assert(handler.get(point(2, 2)).isEmpty)
    handler.put(point(0, 0), "(0, 0)")
    assert(handler.get(point(0, 0)).get == "(0, 0)")
    assert(handler.get(point(1, 1)).get == "(1, 1)")
    handler.delete(point(1, 1))
    assert(handler.get(point(1, 1)).isEmpty)
  }

  test("range test") {
    val handler = fixture
    for (i <- 0 until 10) {
      handler.put(point(i, i), (i, i).toString)
    }
    assert(
      handler.scan(Rectangle(0, 0, 10, 10)).toSet == Range(0, 10).map(i => (RoundedPoint(i, i), (i, i).toString)).toSet)
    assert(
      handler.scan(Rectangle(4, 4, 7, 7)).toSet == Range(4, 7).map(i => (RoundedPoint(i, i), (i, i).toString)).toSet)
    assert(handler.scan(Rectangle(20, 20, 30, 30)).isEmpty)
  }

  test("k nearest neighbors small") {
    val index = fixture
    for (i <- 4 until 7; j <- 4 until 7) {
      val x = i * 10
      val y = j * 10
      index.put(point(x, y), (x, y).toString)
    }
    testNearestSet(index.nearestNeighbor(point(50, 50), 100),
      (50, 50), (40, 40), (40, 50), (40, 60), (50, 40), (50, 60), (60, 40), (60, 50), (60, 60))
  }

  test("k nearest neighbors") {
    val index = fixture
    for (i <- 0 until 10) {
      for (j <- 0 until 10) {
        val x = i * 10
        val y = j * 10
        index.put(point(x, y), (x, y).toString)
      }
    }
    testNearest(index.nearestNeighbor(point(0, 0), 1), (0, 0))
    testNearest(index.nearestNeighbor(point(1, 1), 1), (0, 0))
    testNearest(index.nearestNeighbor(point(0, 4), 1), (0, 0))
    testNearest(index.nearestNeighbor(point(4, 0), 1), (0, 0))

    testNearest(index.nearestNeighbor(point(50, 50), 1), (50, 50))
    testNearest(index.nearestNeighbor(point(52, 64), 4), (50, 60), (50, 70), (60, 60), (60, 70))
    testNearestSet(index.nearestNeighbor(point(50, 50), 9),
      (50, 50), (40, 40), (40, 50), (40, 60), (50, 40), (50, 60), (60, 40), (60, 50), (60, 60))
  }

  def point(tuple: (Int, Int)): RoundedPoint = point(tuple._1, tuple._2)

  def point(x: Int, y: Int) = new RoundedPoint(x, y)
}
