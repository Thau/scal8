package com.gonzaloquero.scal8

class MockKeyDispatcher extends KeyDispatcher {
  val pressedKeys: Array[Boolean] = Array.ofDim(0xF)

  def set(code: Byte, pressed: Boolean): Unit = pressedKeys(code) = pressed
  override def isPressed(code: Byte): Boolean = pressedKeys(code)
}
