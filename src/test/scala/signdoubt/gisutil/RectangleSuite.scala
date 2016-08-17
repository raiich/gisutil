package signdoubt.gisutil

import org.scalatest.FunSuite

class RectangleSuite extends FunSuite {
  test("test overlap") {
    val center = Rectangle(10, 10, 20, 20)
    testNotOverlap(center, Rectangle(0, 0, 10, 10)) // left down
    testNotOverlap(center, Rectangle(10, 0, 20, 10)) // down
    testNotOverlap(center, Rectangle(20, 0, 30, 10)) // right down
    testNotOverlap(center, Rectangle(20, 10, 30, 20)) // right
    testNotOverlap(center, Rectangle(20, 20, 30, 30)) // right up
    testNotOverlap(center, Rectangle(10, 20, 20, 30)) // up
    testNotOverlap(center, Rectangle(0, 20, 10, 30)) // left up
    testNotOverlap(center, Rectangle(0, 10, 10, 20)) // left

    testNotOverlap(center, Rectangle(30, 30, 40, 40)) // outer away

    testOverlap(center, Rectangle(11, 11, 19, 19)) // included
    testOverlap(center, Rectangle(10, 10, 19, 19)) // touched left down
    testOverlap(center, Rectangle(11, 11, 20, 20)) // touched right up
    testOverlap(center, Rectangle(15, 15, 25, 25)) // cross right up
    testOverlap(center, Rectangle(15, 5, 25, 15)) // cross right down
    testOverlap(center, Rectangle(5, 15, 15, 25)) // cross left up
    testOverlap(center, Rectangle(5, 5, 15, 15)) // cross left down
  }

  def testOverlap(center: Rectangle, other: Rectangle) {
    assert(center.overlap(other))
    assert(other.overlap(center))
  }

  def testNotOverlap(center: Rectangle, other: Rectangle) {
    assert(!center.overlap(other))
    assert(!other.overlap(center))
  }
}
