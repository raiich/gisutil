package signdoubt.gisutil

package object core {
  /**
    * type ZValue represents the interleaved value with (x, y) for each bit.
    */
  type ZValue = Long
  val intBitSize: Int = 32
  private val one = 0x1L

  /**
    * Interleaves with each bit of (x, y).
    * If x, y = 1110, 0001, then interleaved value is 10101001.
    * TODO https://graphics.stanford.edu/~seander/bithacks.html
    *
    * @param x x value
    * @param y y value
    * @return interleaved ZValue
    */
  def bitwiseZip(x: Int, y: Int): ZValue = naiveInterleaveBits(x, y)

  private def naiveInterleaveBits(x: Int, y: Int): ZValue = {
    val mask = one
    var result = 0L
    for (i <- 0 until intBitSize) {
      result |= (y & (mask << i)) << i
      result |= (x & (mask << i)) << (i + 1)
    }
    result
  }

  /**
    * If interleaved value is 10101001, then x, y = 1110, 0001.
    *
    * @param bits Long representation of bits
    * @return de-interleaved value
    */
  def bitwiseUnzip(bits: ZValue): (Int, Int) = naiveDeInterleaveBits(bits)

  private def naiveDeInterleaveBits(bits: ZValue): (Int, Int) = {
    val mask = one
    var (x, y) = (0L, 0L)
    for (i <- 0 until intBitSize) {
      y |= (bits & (mask << (i * 2))) >>> i
      x |= (bits & (mask << (i * 2 + 1))) >>> i + 1
    }
    (x.asInstanceOf[Int], y.asInstanceOf[Int])
  }

  object ZValue {
    def apply(point: RoundedPoint): ZValue = apply(point.x, point.y)

    def apply(x: Int, y: Int): ZValue = bitwiseZip(x, y)
  }

}
