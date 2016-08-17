package signdoubt.gisutil.core

import org.scalatest.FunSuite

class PackageSuite extends FunSuite {

  def testZipUnzip(base: Long, x: Int, y: Int) {
    assert(bitwiseZip(x, y) == base)
    assert(bitwiseUnzip(base) == (x, y))
  }

  test("check zip and unzip") {
    testZipUnzip(0x0L, 0x00000000, 0x00000000)
    testZipUnzip(0x5555555555555555L, 0x00000000, 0xffffffff)
    testZipUnzip(0xffffffffffffffffL, 0xffffffff, 0xffffffff)
    testZipUnzip(0xaaaaaaaaaaaaaaaaL, 0xffffffff, 0x00000000)
    testZipUnzip(0x00c0c0c0c0c0c0c0L, 0x08888888, 0x08888888)
    testZipUnzip(0x2aaaaaaaaaaaaaaaL, 0x7fffffff, 0x00000000)
    testZipUnzip(0x3fffffffffffffffL, 0x7fffffff, 0x7fffffff)
    testZipUnzip(0x7fffffffffffffffL, 0x7fffffff, 0xffffffff)
  }
}