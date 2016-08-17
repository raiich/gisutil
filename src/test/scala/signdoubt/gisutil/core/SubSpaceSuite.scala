package signdoubt.gisutil.core

import org.scalatest.FunSuite
import signdoubt.gisutil.Rectangle

import scala.collection.SortedSet

class SubSpaceSuite extends FunSuite {

  test("subspace range") {
    testRange("00", "11")
    testRange("0000", "0011")
    testRange("001100", "001111")
    testRange("000011", "001100")
    testRange("0011", "1100")
  }

  def testRange(lower: String, upper: String) {
    val low = bitwiseUnzip(BitArray64(lower).longBits)
    val up = bitwiseUnzip(BitArray64(upper).longBits)
    assert(ss(lower).rectangle == Rectangle.normalized(low._1, low._2, up._1, up._2))
  }

  def ss(id: String): MaterializedSubSpace = ss(BitArray64(id))

  def ss(id: BitArray64) = new MaterializedSubSpace(id, SortedSet())

  test("restore Z-value") {
    assert(bitwiseUnzip(ss("0011").zValue) == (0x40000000, 0x40000000))
    assert(bitwiseUnzip(ss("1100").zValue) == (0x80000000, 0x80000000))
  }

  test("Z-value split") {
    val rootSpaceName = BitArray64("00")
    assert(ss(rootSpaceName).split()(0).spaceName == ss("000").spaceName)
    assert(ss(rootSpaceName).split()(1).spaceName == ss("001").spaceName)
    assert(ss("000").split()(0).spaceName == ss("0000").spaceName)
    assert(ss("000").split()(1).spaceName == ss("0001").spaceName)
    assert(ss("001").split()(0).spaceName == ss("0010").spaceName)
    assert(ss("001").split()(1).spaceName == ss("0011").spaceName)
  }
}
