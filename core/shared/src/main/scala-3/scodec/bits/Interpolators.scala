package scodec.bits

import scala.quoted._

/**
  * Provides the `hex` string interpolator, which returns `ByteVector` instances from hexadecimal strings.
  *
  * @example {{{
  * scala> val b = hex"deadbeef"
  * val b: scodec.bits.ByteVector = ByteVector(4 bytes, 0xdeadbeef)
  * }}}
  */
extension (inline ctx: StringContext) inline def hex (inline args: ByteVector*): ByteVector =
  ${hexInterpolator('ctx, 'args)}

private def hexInterpolator(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[ByteVector]])(using Quotes): Expr[ByteVector] = {
  (strCtxExpr, argsExpr) match {
    case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args)) =>
      val partValues: Seq[String] = parts.map { p =>
        val part = p.unliftOrError
        if (ByteVector.fromHex(part).isEmpty)
          quotes.reflect.report.error("hexadecimal string literal may only contain characters [0-9a-fA-f]", p)
        part
      }
      if (partValues.size == 1)
        '{ByteVector.fromValidHex(${Expr(partValues.head)})}
      else {
        val init: Expr[StringBuilder] = '{ new StringBuilder().append(${Expr(partValues.head)}) }
        val bldr: Expr[StringBuilder] = args.zip(partValues.tail).foldLeft(init) { case (sb, (arg, part)) =>
          '{$sb.append($arg.toHex).append(${Expr(part)})}
        }
        '{ByteVector.fromValidHex($bldr.toString)}
      }
  }
}

/**
  * Provides the `bin` string interpolator, which returns `BitVector` instances from binary strings.
  *
  * @example {{{
  * scala> val b = bin"1010101010"
  * val b: scodec.bits.BitVector = BitVector(10 bits, 0xaa8)
  * }}}
  */
extension (inline ctx: StringContext) inline def bin (inline args: BitVector*): BitVector =
  ${binInterpolator('ctx, 'args)}

private def binInterpolator(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[BitVector]])(using Quotes): Expr[BitVector] = {
  (strCtxExpr, argsExpr) match {
    case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args)) =>
      val partValues: Seq[String] = parts.map { p =>
        val part = p.unliftOrError
        if (BitVector.fromBin(part).isEmpty)
          quotes.reflect.report.error("binary string literal may only contain characters [0, 1]", p)
        part
      }
      if (partValues.size == 1)
        '{BitVector.fromValidBin(${Expr(partValues.head)})}
      else {
        val init: Expr[StringBuilder] = '{ new StringBuilder().append(${Expr(partValues.head)}) }
        val bldr: Expr[StringBuilder] = args.zip(partValues.tail).foldLeft(init) { case (sb, (arg, part)) =>
          '{$sb.append($arg.toBin).append(${Expr(part)})}
        }
        '{BitVector.fromValidBin($bldr.toString)}
      }
  }
}
