package scodec.bits

import scala.quoted._
import scala.quoted.matching._
import scala.util.FromDigits

private[bits] trait BitVectorPlatform { self: BitVector.type =>
  given as FromDigits.WithRadix[BitVector] = BitVectorFromDigits.Instance
}

private[bits] object BitVectorFromDigits {

  class Base extends FromDigits.WithRadix[BitVector] {
    def fromDigits(digits: String, radix: Int): BitVector =
      digitsToBitVector(digits, radix)
  }

  def digitsToBitVector(digits: String, radix: Int): BitVector =
    if (radix == 16) ByteVector.fromValidHex(digits.tail).bits
    else throw FromDigits.MalformedNumber(s"unsupported radix $radix")

  object Instance extends Base {
    override inline def fromDigits(digits: String): BitVector =
      ${digitsToBitVectorMacro('digits, Expr(10))}
    override inline def fromDigits(digits: String, radix: Int): BitVector =
      ${digitsToBitVectorMacro('digits, 'radix)}
  }
}
