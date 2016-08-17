package signdoubt.gisutil.core

/**
  * This class represents bit array up to 64 bit length.
  *
  * @param longBits  Long value representation of bit array
  * @param bitLength size of bit array
  */
case class BitArray64(longBits: Long, bitLength: Int) extends Comparable[BitArray64] {
  private val stringFormat = "%" + BitArray64.bitSize + "s"

  /**
    * Appends a bit of 0 to this [[BitArray64]].
    *
    * @return appended value
    */
  def appendZero: BitArray64 = BitArray64(longBits, bitLength + 1)

  /**
    * Appends a bit of 1 to this [[BitArray64]].
    *
    * @return appended value
    */
  def appendOne: BitArray64 = BitArray64.incremented(longBits, bitLength + 1)

  /**
    * Increments last bit of [[BitArray64]].
    *
    * @return incremented value
    */
  def increment(): BitArray64 = BitArray64.incremented(longBits, bitLength)

  override def compareTo(other: BitArray64): Int = {
    val bitCompare = BitArray64.unsignedCompare(longBits, other.longBits)
    if (bitCompare == 0) {
      bitLength.compareTo(other.bitLength)
    } else {
      bitCompare
    }
  }

  override def toString: String =
    String.format(stringFormat, longBits.toBinaryString).substring(0, bitLength).replace(' ', '0')
}

object BitArray64 {
  private val one = 1L
  private val bitSize = 64

  /**
    * Initial value of [[BitArray64]] with no length.
    */
  private val base = BitArray64(0, 0)

  def apply(string: String): BitArray64 = string.foldLeft(base)(bitAppend)

  private def bitAppend(base: BitArray64, c: Char): BitArray64 = {
    if (c == '1') {
      base.appendOne
    } else if (c == '0') {
      base.appendZero
    } else {
      throw new UnsupportedOperationException("invalid value: " + c)
    }
  }

  private[core] def incremented(bits: Long, size: Int): BitArray64 = BitArray64(increment(bits, size), size)

  private[core] def increment(bits: Long, size: Int): Long = bits + (one << (bitSize - size))

  private[core] def unsignedCompare(bits1: Long, bits2: Long): Int = {
    if (bits1 == bits2) {
      0
    } else if (isUnsignedLarger(bits1, bits2)) {
      1
    } else {
      -1
    }
  }

  private[core] def isUnsignedLarger(larger: Long, smaller: Long): Boolean = {
    if (larger >= 0 && smaller >= 0 || larger < 0 && smaller < 0) {
      larger > smaller
    } else {
      larger < smaller
    }
  }
}
