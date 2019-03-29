package com.gonzaloquero.scal8

import com.gonzaloquero.scal8.UByteImplicits._
import org.scalatest._

class UByteSpec extends FlatSpec with Matchers {
  "getValue" should "return the value of the UByte" in {
    val a: UByte = UByte.of(1)
    a.getValue should be(1.toByte)
  }

  "equals" should "return true for two UBytes of the same value" in {
    UByte.of(1).equals(UByte.of(1)) should be(true)
  }

  "==" should "return true for two UBytes of the same value" in {
    UByte.of(1) == UByte.of(1) should be(true)
  }

  "compare" should "return 0 if both are equal" in {
    val a: UByte = UByte.of(1)
    val b: UByte = UByte.of(1)

    a.compare(b) should be(0)
  }

  "compare" should "return -1 if the first value is lesser" in {
    val a: UByte = UByte.of(1)
    val b: UByte = UByte.of(2)

    a.compare(b) should be(-1)
  }

  "compare" should "return -1 between a value that would be over the sign and a lesser value" in {
    val a: UByte = UByte.of(1)
    val b: UByte = UByte.of(0xFF.toByte) // Could be 255 or -128

    a.compare(b) should be(-1)
  }

  "compare" should "return 1 if the first value is higher" in {
    val a: UByte = UByte.of(2)
    val b: UByte = UByte.of(1)

    a.compare(b) should be(1)
  }

  "compare" should "return 1 between a value that would be over the sign and a lesser value" in {
    val a: UByte = UByte.of(0xFF.toByte) // Could be 255 or -128
    val b: UByte = UByte.of(1)

    a.compare(b) should be(1)
  }

  "compare" should "return 1 between a value that would be over the sign and a lesser value (2)" in {
    val a: UByte = UByte.of(0xFF.toByte) // Could be 255 or -128
    val b: UByte = UByte.of(0xFE.toByte)

    a.compare(b) should be(1)
  }

  "-" should "calculate a - b" in {
    val a: UByte = UByte.of(3)
    val b: UByte = UByte.of(2)

    (a - b) should be(UByte.of(1))
  }

  "-" should "calculate a - b for values over the sign change" in {
    val a: UByte = UByte.of(0xFF.toByte)
    val b: UByte = UByte.of(1)

    (a - b) should be(UByte.of(0xFE.toByte))
  }

  "+" should "calculate a + b" in {
    val a: UByte = UByte.of(3)
    val b: UByte = UByte.of(2)

    (a + b) should be(UByte.of(5))
  }

  "+" should "overflow a + b" in {
    val a: UByte = UByte.of(0xFF.toByte)
    val b: UByte = UByte.of(1)

    (a + b) should be(UByte.of(0))
  }

  "|" should "calculate a | b" in {
    val a: UByte = UByte.of(4)
    val b: UByte = UByte.of(2)

    (a | b) should be(UByte.of(6))
  }

  "&" should "calculate a & b" in {
    val a: UByte = UByte.of(7)
    val b: UByte = UByte.of(6)

    (a & b) should be(UByte.of(6))
  }

  "^" should "calculate a xor b" in {
    val a: UByte = UByte.of(7)
    val b: UByte = UByte.of(6)

    (a ^ b) should be(UByte.of(1))
  }

  ">>>" should "shift a >>> x" in {
    val a: UByte = UByte.of(2)

    (a >>> 1) should be(UByte.of(1)) // 1 bit shift is equivalent to dividing by 2
  }

  ">>>" should "shift a >>> x (2)" in {
    val a: UByte = UByte.of(4)

    (a >>> 2) should be(UByte.of(1)) // 1 bit shift is equivalent to dividing by 2
  }

  ">>>" should "shift a >>> x (3)" in {
    val a: UByte = UByte.of(0xFF.toByte)

    (a >>> 1) should be(UByte.of(0x7F.toByte)) // 1 bit shift is equivalent to dividing by 2
  }

  ">>>" should "shift a >>> x (4)" in {
    val a: UByte = UByte.of(0xFF.toByte)

    (a >>> 2) should be(UByte.of(0x3F.toByte)) // 1 bit shift is equivalent to dividing by 2
  }

  "<<" should "shift a << x" in {
    val a: UByte = UByte.of(2)

    (a << 1) should be(UByte.of(4)) // 1 bit shift is equivalent to multiplying by 2
  }

  "implicits" should "convert an int to a UByte" in {
    val a: UByte = 1

    a should be(UByte.of(1))
  }
}
