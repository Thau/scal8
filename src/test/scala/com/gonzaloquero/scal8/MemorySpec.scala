package com.gonzaloquero.scal8

import com.gonzaloquero.scal8.UByteImplicits._
import org.scalatest._

class MemorySpec extends FlatSpec with Matchers {
  "withProgram" should "provide an memory of the right size" in {
    val memory = Memory.withProgram(Array[UByte]())

    for (i <- 0 until 0xFFF) {
      memory.get(i) should be(UByte.of(0))
    }
  }

  "withProgram" should "save the provided program starting on 0x200" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x00,
        0xE0,
        0xA2,
        0x48
      ))

    for (i <- 0 until 0x200) {
      memory.get(i) should be(UByte.of(0))
    }

    memory.get(0x200) should be(UByte.of(0x00.toByte))
    memory.get(0x201) should be(UByte.of(0xE0.toByte))
    memory.get(0x202) should be(UByte.of(0xA2.toByte))
    memory.get(0x203) should be(UByte.of(0x48.toByte))
  }

  "get" should "return the value in the correspondent memory address" in {
    val memory = Memory.withProgram(Array[UByte](0x48))
    memory.get(0x200) should be(UByte.of(0x48.toByte))
  }

  "set" should "change the value of the memory address" in {
    val memory = Memory.withProgram(Array[UByte]())
    memory.set(0x200, 0x48)
    memory.get(0x200) should be(UByte.of(0x48.toByte))
  }
}
