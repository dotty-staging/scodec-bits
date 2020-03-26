package scodec.bits

import scala.quoted._
import scala.quoted.matching._
import scala.util.FromDigits

private[bits] trait ByteVectorPlatform { self: ByteVector.type =>
  given as FromDigits.WithRadix[ByteVector] = ByteVectorFromDigits.Instance
}

private[bits] object ByteVectorFromDigits {

  class Base extends FromDigits.WithRadix[ByteVector] {
    def fromDigits(digits: String, radix: Int): ByteVector =
      digitsToByteVector(digits, radix)
  }

  def digitsToByteVector(digits: String, radix: Int): ByteVector =
    if (radix == 16) ByteVector.fromValidHex(digits)
    else throw FromDigits.MalformedNumber(s"unsupported radix $radix")

  object Instance extends Base {
    /* Disabled for now as they are generating null values with current dotty head
    override inline def fromDigits(digits: String): ByteVector =
      ${digitsToByteVectorMacro('digits, Expr(10))}
    override inline def fromDigits(digits: String, radix: Int): ByteVector =
      ${digitsToByteVectorMacro('digits, 'radix)}
     */
  }
}
