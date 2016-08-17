package signdoubt.gisutil.core

import org.scalatest.FunSuite
import signdoubt.gisutil.{Rectangle, RoundedPoint, SpatialQueryHandler}

abstract class QueryHandlerSuite extends FunSuite {
  def fixture: SpatialQueryHandler[String]

  def testNearest(iterable: Iterable[(RoundedPoint, String)], expected: (Int, Int)*) {
    assert(iterable.toSeq == expected.map(tuple => (point(tuple), tuple.toString)))
  }

  def point(tuple: (Int, Int)): RoundedPoint = point(tuple._1, tuple._2)

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

  def point(x: Int, y: Int) = new RoundedPoint(x, y)
}
