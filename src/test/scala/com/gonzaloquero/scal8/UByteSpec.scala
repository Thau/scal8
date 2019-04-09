package com.gonzaloquero.scal8

import com.gonzaloquero.scal8.UByteImplicits._
import org.scalatest._

class UByteSpec extends FlatSpec with Matchers {
  "value" should "return the value of the UByte" in {
    val a: UByte = UByte(1)
    a.value should be(1)
  }

  "equals" should "return true for two UBytes of the same value" in {
    UByte(1).equals(UByte(1)) should be(true)
  }

  "==" should "return true for two UBytes of the same value" in {
    UByte(1) == UByte(1) should be(true)
  }

  "compare" should "return 0 if both are equal" in {
    val a: UByte = UByte(1)
    val b: UByte = UByte(1)

    a.compare(b) should be(0)
  }

  "compare" should "return -1 if the first value is lesser" in {
    val a: UByte = UByte(1)
    val b: UByte = UByte(2)

    a.compare(b) should be(-1)
  }

  "compare" should "return -1 between a value that would be over the sign and a lesser value" in {
    val a: UByte = UByte(1)
    val b: UByte = UByte(0xFF) // Could be 255 or -128

    a.compare(b) should be(-1)
  }

  "compare" should "return 1 if the first value is higher" in {
    val a: UByte = UByte(2)
    val b: UByte = UByte(1)

    a.compare(b) should be(1)
  }

  "compare" should "return 1 between a value that would be over the sign and a lesser value" in {
    val a: UByte = UByte(0xFF) // Could be 255 or -128
    val b: UByte = UByte(1)

    a.compare(b) should be(1)
  }

  "compare" should "return 1 between a value that would be over the sign and a lesser value (2)" in {
    val a: UByte = UByte(0xFF) // Could be 255 or -128
    val b: UByte = UByte(0xFE)

    a.compare(b) should be(1)
  }

  "-" should "calculate a - b" in {
    val a: UByte = UByte(3)
    val b: UByte = UByte(2)

    (a - b) should be(UByte(1))
  }

  "-" should "calculate a - b for values over the sign change" in {
    val a: UByte = UByte(0xFF)
    val b: UByte = UByte(1)

    (a - b) should be(UByte(0xFE))
  }

  "+" should "calculate a + b" in {
    val a: UByte = UByte(3)
    val b: UByte = UByte(2)

    (a + b) should be(UByte(5))
  }

  "+" should "overflow a + b" in {
    val a: UByte = UByte(0xFF)
    val b: UByte = UByte(1)

    (a + b) should be(UByte(0))
  }

  "|" should "calculate a | b" in {
    val a: UByte = UByte(4)
    val b: UByte = UByte(2)

    (a | b) should be(UByte(6))
  }

  "&" should "calculate a & b" in {
    val a: UByte = UByte(7)
    val b: UByte = UByte(6)

    (a & b) should be(UByte(6))
  }

  "^" should "calculate a xor b" in {
    val a: UByte = UByte(7)
    val b: UByte = UByte(6)

    (a ^ b) should be(UByte(1))
  }

  ">>>" should "shift a >>> x" in {
    val a: UByte = UByte(2)

    (a >>> 1) should be(UByte(1)) // 1 bit shift is equivalent to dividing by 2
  }

  ">>>" should "shift a >>> x (2)" in {
    val a: UByte = UByte(4)

    (a >>> 2) should be(UByte(1)) // 1 bit shift is equivalent to dividing by 2
  }

  ">>>" should "shift a >>> x (3)" in {
    val a: UByte = UByte(0xFF)

    (a >>> 1) should be(UByte(0x7F)) // 1 bit shift is equivalent to dividing by 2
  }

  ">>>" should "shift a >>> x (4)" in {
    val a: UByte = UByte(0xFF)

    (a >>> 2) should be(UByte(0x3F)) // 1 bit shift is equivalent to dividing by 2
  }

  "<<" should "shift a << x" in {
    val a: UByte = UByte(2)

    (a << 1) should be(UByte(4)) // 1 bit shift is equivalent to multiplying by 2
  }

  "implicits" should "convert an int to a UByte" in {
    val a: UByte = 1

    a should be(UByte(1))
  }
}
