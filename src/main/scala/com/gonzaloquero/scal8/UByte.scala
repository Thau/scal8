package com.gonzaloquero.scal8

import scala.language.implicitConversions

class UByte private (val value: Byte = 0) extends AnyVal with Ordered[UByte] {
  override def compare(that: UByte): Int = {
    val thisValue = if (value < 0) { 256 + value.toInt } else { value }
    val thatValue = if (that.value < 0) { 256 + that.value.toInt } else { that.value }

    thisValue.compare(thatValue)
  }

  override def toString: String = f"0x$value%02x"

  def -(that: UByte): UByte = UByte(value - that.value)
  def +(that: UByte): UByte = UByte(value + that.value)
  def |(that: UByte): UByte = UByte(value | that.value)
  def &(that: UByte): UByte = UByte(value & that.value)
  def ^(that: UByte): UByte = UByte(value ^ that.value)
  def <<(bits: Int): UByte  = UByte(value << bits)

  // This sounds stupid, but we need to remove the MSB after every step because otherwise everything becomes Ints
  // with a lot of leading 1s
  def >>>(bits: Int): UByte = {
    var res = UByte(value)
    for (_ <- 0 until bits) {
      res = UByte((res.value >>> 1) & 0x7F)
    }

    res
  }

}

object UByte {
  def apply(b: Byte): UByte = new UByte(b)
  def apply(b: Int): UByte  = new UByte(b.toByte)
}

object UByteImplicits {
  implicit def Int2UByte(value: Int): UByte = UByte(value)
}
