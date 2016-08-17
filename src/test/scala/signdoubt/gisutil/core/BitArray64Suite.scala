package signdoubt.gisutil.core

import org.scalatest.FunSuite

class BitArray64Suite extends FunSuite {
  test("check string format") {
    testStringFormat("")
    testStringFormat("0")
    testStringFormat("1")
    testStringFormat("00")
    testStringFormat("01")
    testStringFormat("10")
    testStringFormat("11")
    testStringFormat("000")
    testStringFormat("111")
  }

  def testStringFormat(string: String) {
    val bits = BitArray64(string)
    assert(bits.toString == string)
  }

  test("compareTo check") {
    testCompareTo("", "", 0)
    testCompareTo("0", "0", 0)
    testCompareTo("0", "1", -1)
    testCompareTo("1", "0", 1)
    testCompareTo("1", "1", 0)
    testCompareTo("01", "1", -1)
    testCompareTo("01", "0", 1)
    testCompareTo("10", "0", 1)
    testCompareTo("10", "1", 1)
    testCompareTo("10", "11", -1)
    testCompareTo("10", "10", 0)
  }

  def testCompareTo(left: String, right: String, expected: Int) {
    assert(BitArray64(left).compareTo(BitArray64(right)) == expected)
  }

}
