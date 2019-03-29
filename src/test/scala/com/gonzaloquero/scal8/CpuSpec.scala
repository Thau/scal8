package com.gonzaloquero.scal8

import org.scalatest._
import com.gonzaloquero.scal8.UByteImplicits._

class CpuSpec extends FlatSpec with Matchers {
  "00E0" should "clear the graphic memory" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x00,
        0xE0
      ))
    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick()

    for {
      x <- 0 until graphicMemory.width
      y <- 0 until graphicMemory.height
    } {
      graphicMemory.get(x, y) should be(false)
    }
  }

  "1NNN" should "jump to address NNN" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x12, 0x04, // 0x200: Jump to 0x204
        0x00, 0x00, // 0x202: Noop
        0x00, 0xE0 // 0x204: Clear screen
      ))
    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick()
    cpu.tick()

    graphicMemory.get(10, 10) should be(false)
  }

  "2NNN and 00EE" should "call a subroutine and come back" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x22, 0x04, // 0x200: Jump to 0x204
        0x00, 0x00, // 0x202: Noop
        0x00, 0xE0, // 0x204: Clear screen
        0x00, 0xEE, // 0x206: Return to 0x200
        0x00, 0xE0 // 0x208: Clear screen
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)

    cpu.tick() // 0x200: Jumps to 0x204
    cpu.tick() // 0x204: Clears the screen
    graphicMemory.get(10, 10) should be(false)
    graphicMemory.set(10, 10, true)           // We paint again
    cpu.tick()                                // 0x206: Returns
    cpu.tick()                                // 0x202: Noop
    graphicMemory.get(10, 10) should be(true) // If we don't return, 0x208 would clear the screen again.
  }

  // All these tests also test 6XNN
  "3XNN" should "skip the next instruction if VX equals NN" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x30, 0x10, // 0x202: If V0 is 10, skip 0x204
        0x00, 0xE0, // 0x204: Clear screen
        0x00, 0x00 // 0x206: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: V0 is 10, so skip 0x204
    cpu.tick() // 0x206: Noop

    graphicMemory.get(10, 10) should be(true) // We skipped the screen clear
  }

  "3XNN" should "run the next instruction if VX does not equal NN" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x30, 0x11, // 0x202: If V0 is 11, skip 0x204
        0x00, 0xE0, // 0x204: Clear screen
        0x00, 0x00 // 0x206: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: V0 is 11, so don't skip 0x204
    cpu.tick() // 0x204: Clear screen

    graphicMemory.get(10, 10) should be(false)
  }

  "4XNN" should "skip the next instruction if VX does not equal NN" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x40, 0x11, // 0x202: If V0 is 11, skip 0x204
        0x00, 0xE0, // 0x204: Clear screen
        0x00, 0x00 // 0x206: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: V0 is 11, so skip 0x204
    cpu.tick() // 0x206: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "4XNN" should "run the next instruction if VX equals NN" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x40, 0x10, // 0x202: If V0 is 10, skip 0x204
        0x00, 0xE0, // 0x204: Clear screen
        0x00, 0x00 // 0x206: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: V0 is 10, so run 0x204
    cpu.tick() // 0x204: Clear screen

    graphicMemory.get(10, 10) should be(false)
  }

  "5XY0" should "skip the next instruction if VX equals VY" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x61, 0x10, // 0x202: Set V1 to 10
        0x50, 0x10, // 0x204: If V0 is V1, skip 0x206
        0x00, 0xE0, // 0x206: Clear screen
        0x00, 0x00 // 0x208: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Set V1 to 10
    cpu.tick() // 0x204: V0 is V1, so skip 0x206
    cpu.tick() // 0x208: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "5XY0" should "run the next instruction if VX does not equal VY" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x61, 0x11, // 0x202: Set V1 to 11
        0x50, 0x10, // 0x204: If V0 is V1, skip 0x206
        0x00, 0xE0, // 0x206: Clear screen
        0x00, 0x00 // 0x208: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Set V1 to 11
    cpu.tick() // 0x204: V0 is not V1, so run 0x206
    cpu.tick() // 0x206: Clear screen

    graphicMemory.get(10, 10) should be(false)
  }

  "7XNN" should "add NN to VX" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x70, 0x01, // 0x202: Add 1 to V0
        0x30, 0x11, // 0x204: If V0 is 11, skip 0x204
        0x00, 0xE0, // 0x206: Clear screen
        0x00, 0x00 // 0x208: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Add 1 to V0
    cpu.tick() // 0x204: V0 is 11, so skip 0x206
    cpu.tick() // 0x208: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY0" should "set VX to the value of VY" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x61, 0x11, // 0x202: Set V1 to 11
        0x80, 0x10, // 0x204: Set V0 to V1
        0x30, 0x11, // 0x206: If V0 is 11, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Set V1 to 11
    cpu.tick() // 0x204: Set V0 to V1 (11)
    cpu.tick() // 0x206: V0 is 11, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY1" should "set VX to the value of VX | VY" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x61, 0x0F, // 0x202: Set V1 to 0F
        0x80, 0x11, // 0x204: Set V0 to V0 | V1
        0x30, 0x1F, // 0x206: If V0 is 11, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Set V1 to 0F
    cpu.tick() // 0x204: Set V0 to V0 | V1 (1F)
    cpu.tick() // 0x206: V0 is 1F, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY2" should "set VX to the value of VX & VY" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x61, 0x0F, // 0x202: Set V1 to 0F
        0x80, 0x12, // 0x204: Set V0 to V0 & V1
        0x30, 0x00, // 0x206: If V0 is 00, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Set V1 to 0F
    cpu.tick() // 0x204: Set V0 to V0 & V1 (0)
    cpu.tick() // 0x206: V0 is 00, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY3" should "set VX to the value of VX xor VY" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x61, 0x11, // 0x202: Set V1 to 11
        0x80, 0x13, // 0x204: Set V0 to V0 xor V1
        0x30, 0x01, // 0x206: If V0 is 00, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Set V1 to 0F
    cpu.tick() // 0x204: Set V0 to V0 xor V1 (1)
    cpu.tick() // 0x206: V0 is 1, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY4" should "set VX to the value of VX + VY" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x61, 0x01, // 0x202: Set V1 to 1
        0x80, 0x14, // 0x204: Set V0 to V0 + V1
        0x30, 0x11, // 0x206: If V0 is 11, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Set V1 to 1
    cpu.tick() // 0x204: Set V0 to V0 + V1 (11)
    cpu.tick() // 0x206: V0 is 11, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY4" should "set VF to 1 if VX + VY overflows" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0xFF, // 0x200: Set V0 to FF
        0x61, 0x01, // 0x202: Set V1 to 1
        0x80, 0x14, // 0x204: Set V0 to V0 + V1
        0x3F, 0x01, // 0x206: If VF is 1, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to FF
    cpu.tick() // 0x202: Set V1 to 1
    cpu.tick() // 0x204: Set V0 to V0 + V1 (0, carry 1)
    cpu.tick() // 0x206: VF is 1, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY4" should "set VF to 0 if VX + VY overflows" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x00, // 0x200: Set V0 to 0
        0x61, 0x01, // 0x202: Set V1 to 1
        0x80, 0x14, // 0x204: Set V0 to V0 + V1
        0x3F, 0x00, // 0x206: If VF is 0, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 0
    cpu.tick() // 0x202: Set V1 to 1
    cpu.tick() // 0x204: Set V0 to V0 + V1 (1, carry 0)
    cpu.tick() // 0x206: VF is 0, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY5" should "set VX to the value of VX - VY" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x61, 0x01, // 0x202: Set V1 to 1
        0x80, 0x15, // 0x204: Set V0 to V0 - V1
        0x30, 0x0F, // 0x206: If V0 is 0F, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Set V1 to 1
    cpu.tick() // 0x204: Set V0 to V0 - V1 (F)
    cpu.tick() // 0x206: V0 is F, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY5" should "set VF to 0 if there's a borrow" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x01, // 0x200: Set V0 to 0
        0x61, 0x10, // 0x202: Set V1 to 10
        0x80, 0x15, // 0x204: Set V0 to V0 - V1
        0x3F, 0x00, // 0x206: If V0 is 0F, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 0
    cpu.tick() // 0x202: Set V1 to 10
    cpu.tick() // 0x204: Set V0 to V0 - V1
    cpu.tick() // 0x206: VF is 0, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY5" should "set VF to 0 if there's not a borrow" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 0
        0x61, 0x01, // 0x202: Set V1 to 10
        0x80, 0x15, // 0x204: Set V0 to V0 - V1
        0x3F, 0x01, // 0x206: If V0 is 0F, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Set V1 to 1
    cpu.tick() // 0x204: Set V0 to V0 - V1
    cpu.tick() // 0x206: VF is 1, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY6" should "shift VX 1 bit to the right" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x01, // 0x200: Set V0 to 0
        0x80, 0x06, // 0x202: Shift V0
        0x30, 0x00, // 0x204: If V0 is 00, skip 0x206
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to §
    cpu.tick() // 0x202: Shift V0 1 bit to the right (0)
    cpu.tick() // 0x204: VF is 0, so skip 0x206
    cpu.tick() // 0x208: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY6" should "save VX's LSB in VF" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x01, // 0x200: Set V0 to 0
        0x80, 0x06, // 0x202: Shift V0
        0x3F, 0x01, // 0x204: If VF is 1, skip 0x206
        0x00, 0xE0, // 0x206: Clear screen
        0x00, 0x00 // 0x208: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 1
    cpu.tick() // 0x202: Shift V0 1 bit to the right (0)
    cpu.tick() // 0x204: VF is 1, so skip 0x206
    cpu.tick() // 0x208: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY7" should "set VX to the value of VY - VX" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x01, // 0x200: Set V0 to 1
        0x61, 0x10, // 0x202: Set V1 to 10
        0x80, 0x17, // 0x204: Set V0 to V1 - V0
        0x30, 0x0F, // 0x206: If V0 is 0F, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 1
    cpu.tick() // 0x202: Set V1 to 10
    cpu.tick() // 0x204: Set V0 to V1 - V0 (F)
    cpu.tick() // 0x206: V0 is F, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY7" should "set VF to 0 if there's a borrow" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x61, 0x00, // 0x202: Set V1 to 0
        0x80, 0x17, // 0x204: Set V0 to V1 - V0
        0x3F, 0x00, // 0x206: If VF is 0, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Set V1 to 0
    cpu.tick() // 0x204: Set V0 to V1 - V0
    cpu.tick() // 0x206: VF is 0, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XY7" should "set VF to 1 if there's not a borrow" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x00, // 0x200: Set V0 to 0
        0x61, 0x10, // 0x202: Set V1 to 10
        0x80, 0x17, // 0x204: Set V0 to V1 - V0
        0x3F, 0x01, // 0x206: If VF is 0, skip 0x208
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 0
    cpu.tick() // 0x202: Set V1 to 10
    cpu.tick() // 0x204: Set V0 to V1 - V0
    cpu.tick() // 0x206: VF is 1, so skip 0x208
    cpu.tick() // 0x210: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XYE" should "shift VX 1 bit to the left" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x01, // 0x200: Set V0 to 1
        0x80, 0x0E, // 0x202: Shift V0
        0x30, 0x02, // 0x204: If V0 is 2, skip 0x206
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 1
    cpu.tick() // 0x202: Shift V0 1 bit to the left (2)
    cpu.tick() // 0x204: V0 is 2, so skip 0x206
    cpu.tick() // 0x208: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "8XYE" should "save VX's MSB in VF" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0xFF, // 0x200: Set V0 to 255
        0x80, 0x0E, // 0x202: Shift V0
        0x3F, 0x01, // 0x204: If VF is 1, skip 0x206
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x210: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 255
    cpu.tick() // 0x202: Shift V0 1 bit to the left (0xFE)
    cpu.tick() // 0x204: VF is 1, so skip 0x206
    cpu.tick() // 0x208: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "9XY0" should "skip the next instruction if VX does not equal VY" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0x61, 0x11, // 0x202: Set V1 to 11
        0x90, 0x10, // 0x204: If V0 is V1, skip 0x206
        0x00, 0xE0, // 0x206: Clear screen
        0x00, 0x00 // 0x208: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Set V1 to 11
    cpu.tick() // 0x204: V0 is not V1, so skip 0x206
    cpu.tick() // 0x208: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "BNNN" should "jump to address NNN + V0" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x02, // 0x200: Set V0 to 10
        0xB2, 0x06, // 0x202: Jump to 0x206 + V0 (0x208)
        0x00, 0x00, // 0x204: Noop
        0x00, 0x00, // 0x206: Noop
        0x00, 0xE0 // 0x208: Clear screen
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 10
    cpu.tick() // 0x202: Jump to 0x208
    cpu.tick() // 0x208: Clear screen

    graphicMemory.get(10, 10) should be(false)
  }

  "DXYN and ANNN" should "draw a sprite in coords (VX, VY) with a width of 8 pixels and a height of N" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x01, // 0x200: Set V0 to 1
        0x61, 0x01, // 0x202: Set V1 to 1
        0xA2, 0x08, // 0x204: Sets I to 0x208
        0xD0, 0x12, // 0x206: Draws the sprite in (1, 1), with a height of two
        0x55, 0xAA // 0x208: Sprite: First row is 01010101, second is 10101010
      ))

    val graphicMemory = new GraphicMemory

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 1
    cpu.tick() // 0x202: Set V1 to 1
    cpu.tick() // 0x204: Set I to 0x208
    cpu.tick() // 0x206: Draw

    graphicMemory.get(1, 1) should be(false)
    graphicMemory.get(2, 1) should be(true)
    graphicMemory.get(3, 1) should be(false)
    graphicMemory.get(4, 1) should be(true)
    graphicMemory.get(5, 1) should be(false)
    graphicMemory.get(6, 1) should be(true)
    graphicMemory.get(7, 1) should be(false)
    graphicMemory.get(8, 1) should be(true)

    graphicMemory.get(1, 2) should be(true)
    graphicMemory.get(2, 2) should be(false)
    graphicMemory.get(3, 2) should be(true)
    graphicMemory.get(4, 2) should be(false)
    graphicMemory.get(5, 2) should be(true)
    graphicMemory.get(6, 2) should be(false)
    graphicMemory.get(7, 2) should be(true)
    graphicMemory.get(8, 2) should be(false)
  }

  "DXYN" should "sets VF to 1 if it unsets any set pixel" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x01, // 0x200: Set V0 to 1
        0x61, 0x01, // 0x202: Set V1 to 1
        0xA2, 0x0E, // 0x204: Sets I to 0x20E
        0xD0, 0x12, // 0x206: Draws the sprite in (1, 1), with a height of two
        0x3F, 0x01, // 0x208: If VF is 1, skip 0x210
        0x00, 0xE0, // 0x20A: Clear screen
        0x00, 0x00, // 0x20C: Noop
        0x55, 0xAA // 0x20E: Sprite: First row is 01010101, second is 10101010
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(1, 1, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 1
    cpu.tick() // 0x202: Set V1 to 1
    cpu.tick() // 0x204: Set I to 0x214
    cpu.tick() // 0x206: Draw
    cpu.tick() // 0x208: Drawing unsets (1, 1), so VF is 1, and so, we skip 0x210
    cpu.tick() // 0x20C: Noop

    graphicMemory.get(2, 1) should be(true)
  }

  "EX9E" should "skip the next instruction if the key stored in VX is pressed" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x01, // 0x200: Set V0 to 1
        0xE0, 0x9E, // 0x202: If the key in V0 is pressed, skip 0x204
        0x00, 0xE0, // 0x204: Clear screen
        0x00, 0x00 // 0x206: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val keyDispatcher = new MockKeyDispatcher
    keyDispatcher.set(1, pressed = true)

    val cpu = new Cpu(memory, graphicMemory, keyDispatcher)
    cpu.tick() // 0x200: Set V0 to 1
    cpu.tick() // 0x202: Skip 0x204 because Key 1 is pressed
    cpu.tick() // 0x206: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "EX9E" should "not skip the next instruction if the key stored in VX is not pressed" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x01, // 0x200: Set V0 to 1
        0xE0, 0x9E, // 0x202: If the key in V0 is pressed, skip 0x204
        0x00, 0xE0, // 0x204: Clear screen
        0x00, 0x00 // 0x206: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val keyDispatcher = new MockKeyDispatcher
    keyDispatcher.set(1, pressed = false)

    val cpu = new Cpu(memory, graphicMemory, keyDispatcher)
    cpu.tick() // 0x200: Set V0 to 1
    cpu.tick() // 0x202: Don't skip 0x204 because Key 1 is pressed
    cpu.tick() // 0x204: Clear screen

    graphicMemory.get(10, 10) should be(false)
  }

  "EXA1" should "skip the next instruction if the key stored in VX is not pressed" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x01, // 0x200: Set V0 to 1
        0xE0, 0xA1, // 0x202: If the key in V0 is not pressed, skip 0x204
        0x00, 0xE0, // 0x204: Clear screen
        0x00, 0x00 // 0x206: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val keyDispatcher = new MockKeyDispatcher
    keyDispatcher.set(1, pressed = false)

    val cpu = new Cpu(memory, graphicMemory, keyDispatcher)
    cpu.tick() // 0x200: Set V0 to 1
    cpu.tick() // 0x202: Skip 0x204 because Key 1 is not pressed
    cpu.tick() // 0x206: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "EXA1" should "not skip the next instruction if the key stored in VX is pressed" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x01, // 0x200: Set V0 to 1
        0xE0, 0xA1, // 0x202: If the key in V0 is not pressed, skip 0x204
        0x00, 0xE0, // 0x204: Clear screen
        0x00, 0x00 // 0x206: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val keyDispatcher = new MockKeyDispatcher
    keyDispatcher.set(1, pressed = true)

    val cpu = new Cpu(memory, graphicMemory, keyDispatcher)
    cpu.tick() // 0x200: Set V0 to 1
    cpu.tick() // 0x202: Don't skip 0x204 because Key 1 is pressed
    cpu.tick() // 0x204: Clear screen

    graphicMemory.get(10, 10) should be(false)
  }

  "FX15 and FX07" should "set DT to VX and VX to DT" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x10, // 0x200: Set V0 to 10
        0xF0, 0x15, // 0x202: Set DT to VX
        0xF0, 0x07, // 0x204: Set VX to DT
        0x30, 0x0F, // 0x206: Skip 0x208 if V0 is 0F
        0x00, 0xE0, // 0x208: Clear screen
        0x00, 0x00 // 0x20A: Noop
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick()      // 0x200: Set V0 to 10
    cpu.tick()      // 0x202: Set DT to V0 (10)
    cpu.clockTick() // Tick the clock, makes DT go down to F
    cpu.tick()      // 0x204: Set V0 to DT (F)
    cpu.tick()      // 0x206: Skip 0x208 because V0 is F
    cpu.tick()      // 0x20A: Noop

    graphicMemory.get(10, 10) should be(true)
  }

  "FX1E" should "add VX to I" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x01, // 0x200: Set V0 to 1
        0x61, 0x01, // 0x202: Set V1 to 1
        0x62, 0x0C, // 0x204: Set V2 to C
        0xA2, 0x00, // 0x206: Sets I to 0x200
        0xF2, 0x1E, // 0x208: Adds V2 to I (0x20C)
        0xD0, 0x12, // 0x20A: Draws the sprite in (1, 1), with a height of two
        0x55, 0xAA // 0x20C: Sprite: First row is 01010101, second is 10101010
      ))

    val graphicMemory = new GraphicMemory

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 1
    cpu.tick() // 0x202: Set V1 to 1
    cpu.tick() // 0x204: Set V2 to C
    cpu.tick() // 0x206: Set I to 0x200
    cpu.tick() // 0x208: Add V2 to I (0x20C)
    cpu.tick() // 0x20A: Draw

    graphicMemory.get(2, 1) should be(true)
  }

  "FX55" should "store V0 to VX in memory starting at I" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0x60, 0x55, // 0x200: Set V0 to 0x55
        0x61, 0xAA, // 0x202: Set V1 to 0xAA
        0x62, 0x01, // 0x204: Set V2 to 1
        0x63, 0x01, // 0x206: Set V3 to 1
        0xA3, 0x00, // 0x208: Sets I to 0x300
        0xF1, 0x55, // 0x20A: Store V0 and V1 starting at I
        0xD2, 0x32 // 0x20C: Draws the sprite in (1, 1), with a height of two
      ))

    val graphicMemory = new GraphicMemory

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set V0 to 0x55
    cpu.tick() // 0x202: Set V1 to 0xAA
    cpu.tick() // 0x204: Set V2 to 1
    cpu.tick() // 0x206: Set V3 to 1
    cpu.tick() // 0x208: Set I to 0x300
    cpu.tick() // 0x20A: Store V0 and V1 starting at I (creates the same alternate pattern of pixels)
    cpu.tick() // 0x20C: Draw the sprite

    graphicMemory.get(1, 1) should be(false)
    graphicMemory.get(2, 1) should be(true)
    graphicMemory.get(3, 1) should be(false)
    graphicMemory.get(4, 1) should be(true)
    graphicMemory.get(5, 1) should be(false)
    graphicMemory.get(6, 1) should be(true)
    graphicMemory.get(7, 1) should be(false)
    graphicMemory.get(8, 1) should be(true)

    graphicMemory.get(1, 2) should be(true)
    graphicMemory.get(2, 2) should be(false)
    graphicMemory.get(3, 2) should be(true)
    graphicMemory.get(4, 2) should be(false)
    graphicMemory.get(5, 2) should be(true)
    graphicMemory.get(6, 2) should be(false)
    graphicMemory.get(7, 2) should be(true)
    graphicMemory.get(8, 2) should be(false)
  }

  "FX65" should "fill V0 to VX with memory starting at I" in {
    val memory = Memory.withProgram(
      Array[UByte](
        0xA2, 0x0C, // 0x200: Sets I to 0x20C
        0xF1, 0x65, // 0x202: Fill V0 and V1 with memory starting at I
        0x30, 0x01, // 0x204: Skip 0x206 if V0 is 1
        0x00, 0xE0, // 0x206: Clear screen
        0x31, 0x01, // 0x208: Skip 0x20A if V1 is 1
        0x00, 0xE0, // 0x20A: Clear screen
        0x01, 0x01 // 0x20C: Values to get into the registers
      ))

    val graphicMemory = new GraphicMemory
    graphicMemory.set(10, 10, true)

    val cpu = new Cpu(memory, graphicMemory, new MockKeyDispatcher)
    cpu.tick() // 0x200: Set I to 0x20C
    cpu.tick() // 0x202: Fill V0 and V1 with memory starting at I (1 and 1)
    cpu.tick() // 0x204: Skip 0x206 because V0 == 1
    cpu.tick() // 0x208: Skip 0x20A because V01== 1

    graphicMemory.get(10, 10) should be(true)
  }
}
