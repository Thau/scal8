package com.gonzaloquero.scal8

import scalafx.scene.input.{KeyCode, KeyEvent}

object FXKeyDispatcher extends KeyDispatcher {
  private val keyEquivalences: Map[KeyCode, Int] = Map(
    KeyCode.Digit1 -> 0x1,
    KeyCode.Digit2 -> 0x2,
    KeyCode.Digit3 -> 0x3,
    KeyCode.Digit4 -> 0xC,
    KeyCode.Q      -> 0x4,
    KeyCode.W      -> 0x5,
    KeyCode.E      -> 0x6,
    KeyCode.R      -> 0xD,
    KeyCode.A      -> 0x7,
    KeyCode.S      -> 0x8,
    KeyCode.D      -> 0x9,
    KeyCode.F      -> 0xE,
    KeyCode.Z      -> 0xA,
    KeyCode.X      -> 0x0,
    KeyCode.C      -> 0xB,
    KeyCode.V      -> 0xF
  )

  val pressedKeys: Array[Boolean] = Array.ofDim(0xF)

  def dispatchKeyEvent(ge: KeyEvent): Unit = ge match {
    case ge if !keyEquivalences.contains(ge.code)   => ()
    case ge if ge.eventType == KeyEvent.KeyPressed  => pressedKeys(keyEquivalences(ge.code) - 1) = true
    case ge if ge.eventType == KeyEvent.KeyReleased => pressedKeys(keyEquivalences(ge.code) - 1) = false
    case _                                          => ()
  }

  def isPressed(code: Byte): Boolean = pressedKeys(code - 1)
}
