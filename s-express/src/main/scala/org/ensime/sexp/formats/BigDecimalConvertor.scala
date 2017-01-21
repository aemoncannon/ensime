// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package org.ensime.sexp.formats

class BigDecimalConvertor[T](
    val to: T => BigDecimal,
    val from: BigDecimal => T
) {
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
  // an implicit already exists from many of these types to BigDecimal
  implicit val IntBigConv: BigDecimalConvertor[Int] = new BigDecimalConvertor[Int](identity, _.intValue())
  implicit val LongBigConv: BigDecimalConvertor[Long] = new BigDecimalConvertor[Long](identity, _.longValue())
  implicit val FloatBigConv: BigDecimalConvertor[Float] = new BigDecimalConvertor[Float](identity, _.floatValue()) {
    override def isPosInf(t: Float) = t.isPosInfinity
    override def PosInf = Float.PositiveInfinity
    override def isNegInf(t: Float) = t.isNegInfinity
    override def NegInf = Float.NegativeInfinity
    override def isNaN(t: Float) = t.isNaN
    override def NaN = Float.NaN
  }
  implicit val DoubleBigConv: BigDecimalConvertor[Double] = new BigDecimalConvertor[Double](identity, _.doubleValue()) {
    override def isPosInf(t: Double) = t.isPosInfinity
    override def PosInf = Double.PositiveInfinity
    override def isNegInf(t: Double) = t.isNegInfinity
    override def NegInf = Double.NegativeInfinity
    override def isNaN(t: Double) = t.isNaN
    override def NaN = Double.NaN
  }
  implicit val ByteBigConv: BigDecimalConvertor[Byte] = new BigDecimalConvertor[Byte](identity, _.byteValue())
  implicit val ShortBigConv: BigDecimalConvertor[Short] = new BigDecimalConvertor[Short](identity, _.shortValue())
  implicit val BigIntBigConv: BigDecimalConvertor[BigInt] = new BigDecimalConvertor[BigInt](BigDecimal.apply, _.toBigInt())
  implicit val BigDecimalBigConv: BigDecimalConvertor[BigDecimal] = new BigDecimalConvertor[BigDecimal](identity, identity)
}
