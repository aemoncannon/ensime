package org.ensime.sexp.formats

import collection.BitSet
import collection.{ immutable => im }

class BigDecimalConvertor[T](
    val to: T => BigDecimal,
    val from: BigDecimal => T) {
  protected def unsupported(message: String) =
    throw new UnsupportedOperationException(message)

  def isPosInf(t: T): Boolean = false
  def PosInf: T = unsupported("Positive infinity")
  def isNegInf(t: T): Boolean = false
  def NegInf: T = unsupported("Negative infinity")
  def isNaN(t: T): Boolean = false
  def NaN: T = unsupported("NaN")
}

object BigDecimalConvertor {
  implicit val IntBigConv = new BigDecimalConvertor[Int](BigDecimal.apply, _.intValue)
  implicit val LongBigConv = new BigDecimalConvertor[Long](BigDecimal.apply, _.longValue)
  // no float => BigDecimal in scala 2.9
  implicit val FloatBigConv = new BigDecimalConvertor[Float](f => BigDecimal(f.doubleValue), _.floatValue) {
    override def isPosInf(t: Float) = t.isPosInfinity
    override def PosInf = Float.PositiveInfinity
    override def isNegInf(t: Float) = t.isNegInfinity
    override def NegInf = Float.NegativeInfinity
    override def isNaN(t: Float) = t.isNaN
    override def NaN = Float.NaN
  }
  implicit val DoubleBigConv = new BigDecimalConvertor[Double](BigDecimal.apply, _.doubleValue) {
    override def isPosInf(t: Double) = t.isPosInfinity
    override def PosInf = Double.PositiveInfinity
    override def isNegInf(t: Double) = t.isNegInfinity
    override def NegInf = Double.NegativeInfinity
    override def isNaN(t: Double) = t.isNaN
    override def NaN = Double.NaN
  }
  // no byte or short => BigDecimal in 2.9
  implicit val ByteBigConv = new BigDecimalConvertor[Byte](b => BigDecimal(b.intValue), _.byteValue)
  implicit val ShortBigConv = new BigDecimalConvertor[Short](s => BigDecimal(s.intValue), _.shortValue)
  implicit val BigIntBigConv = new BigDecimalConvertor[BigInt](BigDecimal.apply, _.toBigInt)
  implicit val BigDecimalBigConv = new BigDecimalConvertor[BigDecimal](identity, identity)
}
