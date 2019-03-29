package com.gonzaloquero.scal8

class GraphicMemory {
  val width                         = 64
  val height                        = 32
  val memory: Array[Array[Boolean]] = Array.ofDim[Boolean](width, height)

  def get(x: Int, y: Int): Boolean              = { memory(x)(y) }
  def set(x: Int, y: Int, value: Boolean): Unit = { memory(x)(y) = value }
  def clear(): Unit = {
    for {
      x <- memory.indices
      y <- memory(x).indices
    } {
      set(x, y, false)
    }
  }
}
