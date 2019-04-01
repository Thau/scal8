package com.gonzaloquero.scal8

import scala.language.implicitConversions

class UByte private (value: Byte = 0) extends Ordered[UByte] {
  def getValue: Byte = { value }
  override def compare(that: UByte): Int = {
    val thisValue = if (value < 0) { 256 + value.toInt } else { value }
    val thatValue = if (that.getValue < 0) { 256 + that.getValue.toInt } else { that.getValue }

    thisValue.compare(thatValue)
  }

  override def equals(obj: Any): Boolean = obj match {
    case byte: UByte =>
      value.equals(byte.getValue)
    case _ =>
      false
  }

  override def toString: String = f"0x$value%02x"

  def -(that: UByte): UByte = {
    UByte.of((value - that.getValue).toByte)
  }

  def +(that: UByte): UByte = {
    UByte.of((value + that.getValue).toByte)
  }

  def |(that: UByte): UByte = {
    UByte.of((value | that.getValue).toByte)
  }

  def &(that: UByte): UByte = {
    UByte.of((value & that.getValue).toByte)
  }

  def ^(that: UByte): UByte = {
    UByte.of((value ^ that.getValue).toByte)
  }

  // This sounds stupid, but we need to remove the MSB after every step because otherwise everything becomes Ints
  // with a lot of leading 1s
  def >>>(bits: Int): UByte = {
    var res = UByte.of(value)
    for (_ <- 0 until bits) {
      res = UByte.of(((res.getValue >>> 1).toByte & 0x7F.toByte).toByte)
    }
    res
  }

  def <<(bits: Int): UByte = {
    UByte.of((value << bits).toByte)
  }
}

object UByte {
  def of(b: Byte): UByte = {
    new UByte(b)
  }
}

object UByteImplicits {
  implicit def Int2UByte(value: Int): UByte = UByte.of(value.toByte)
}
