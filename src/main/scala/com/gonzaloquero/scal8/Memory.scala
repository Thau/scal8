package com.gonzaloquero.scal8

import com.gonzaloquero.scal8.UByteImplicits._

class Memory {
  val values: Array[UByte] = new Array[UByte](0xFFF)
  values.indices.foreach(values(_) = 0)

  def get(index: Int): UByte              = values(index)
  def set(index: Int, value: UByte): Unit = values(index) = value
}

object Memory {
  def withProgram(iterable: Iterable[UByte]): Memory = {
    val memory = new Memory()
    iterable.zipWithIndex.foreach { case (value, index) => memory.set(0x200 + index, value) }

    memory
  }
}
