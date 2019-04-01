package com.gonzaloquero.scal8

class GraphicMemory extends Traversable[(Int, Int, Boolean)] {
  val width                         = 64
  val height                        = 32
  val memory: Array[Array[Boolean]] = Array.ofDim[Boolean](width, height)

  def get(x: Int, y: Int): Boolean              = { memory(x)(y) }
  def set(x: Int, y: Int, value: Boolean): Unit = { memory(x)(y) = value }
  def clear(): Unit                             = foreach { case (x, y, _) => set(x, y, false) }

  override def foreach[U](f: ((Int, Int, Boolean)) => U): Unit = {
    for (y <- 0 until height; x <- 0 until width) {
      f(x, y, get(x, y))
    }
  }
}
