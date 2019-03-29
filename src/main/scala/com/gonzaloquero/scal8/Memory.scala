package com.gonzaloquero.scal8

import com.gonzaloquero.scal8.UByteImplicits._

class Memory {
  val values: Array[UByte] = new Array[UByte](0xFFF)

  for (i <- values.indices) {
    values(i) = 0
  }

  def get(index: Int): UByte = {
    values(index)
  }

  def set(index: Int, value: UByte): Unit = {
    values(index) = value
  }
}

object Memory {
  def withProgram(iterable: Iterable[UByte]): Memory = {
    val memory = new Memory()

    var index = 0x200
    for (value <- iterable) {
      memory.set(index, value)
      index += 1
    }

    memory
  }
}
