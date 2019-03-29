package com.gonzaloquero.scal8

import org.scalatest._

class GraphicMemorySpec extends FlatSpec with Matchers {
  "constructor" should "provide an initialised graphic memory" in {
    val graphicMemory = new GraphicMemory()

    for {
      x <- 0 until graphicMemory.width
      y <- 0 until graphicMemory.height
    } {
      graphicMemory.get(x, y) should be(false)
    }
  }

  "set" should "change the status of a pixel" in {
    val graphicMemory = new GraphicMemory()
    graphicMemory.set(10, 10, true)
    graphicMemory.get(10, 10) should be(true)
  }

  "clear" should "clear the memory" in {
    val graphicMemory = new GraphicMemory()
    graphicMemory.set(10, 10, true)
    graphicMemory.clear()

    for {
      x <- 0 until graphicMemory.width
      y <- 0 until graphicMemory.height
    } {
      graphicMemory.get(x, y) should be(false)
    }
  }
}
