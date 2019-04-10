package com.gonzaloquero.scal8

import scala.util.Random

case class OpCode(operation: Byte, vy: Byte, vx: Byte, modifier: Byte, addr: Short, value: UByte)

class Cpu(memory: Memory, graphicMemory: GraphicMemory, keyDispatcher: KeyDispatcher) {
  var addressI: Short         = 0
  var programCounter: Short   = 0x200
  var stack: List[Short]      = List()
  val registers: Array[UByte] = (0 until 16).map(_ => UByte(0)).toArray
  var delayTimer: UByte       = UByte(0)

  def tick(): Unit = {
    val opCode = getNextOpCode
    executeOpCode(opCode)
  }

  def clockTick(): Unit = {
    if (delayTimer > UByte(0)) {
      delayTimer = delayTimer - UByte(1)
    }
  }

  private def getNextOpCode: OpCode = {
    val firstPart  = memory.get(programCounter).value << 8
    val secondPart = memory.get(programCounter + 1).value & 0x00FF

    advanceProgramCounter()
    val opCode = ((firstPart | secondPart).toShort & 0x0000FFFF).toShort

    val op       = ((opCode & 0xF000) >>> 12).toByte
    val modifier = (opCode & 0x000F).toByte
    val vx       = ((opCode & 0x0F00) >>> 8).toByte
    val vy       = ((opCode & 0x00F0) >>> 4).toByte
    val addr     = (opCode & 0x0FFF).toShort
    val value    = UByte(opCode & 0x00FF)

    OpCode(op, vx, vy, modifier, addr, value)
  }

  private def advanceProgramCounter(): Unit = programCounter = (programCounter + 2).toShort

  private def executeOpCode(opCode: OpCode): Unit = opCode match {
    case OpCode(0x0, _, _, _, 0x000, _)  => () // Noop
    case OpCode(0x0, _, _, _, 0x0E0, _)  => opCode00E0()
    case OpCode(0x0, _, _, _, 0x0EE, _)  => opCode00EE()
    case OpCode(0x1, _, _, _, addr, _)   => opCode1NNN(addr)
    case OpCode(0x2, _, _, _, addr, _)   => opCode2NNN(addr)
    case OpCode(0x3, vx, _, _, _, value) => opCode3XNN(vx, value)
    case OpCode(0x4, vx, _, _, _, value) => opCode4XNN(vx, value)
    case OpCode(0x5, vx, vy, 0x0, _, _)  => opCode5XY0(vx, vy)
    case OpCode(0x6, vx, _, _, _, value) => opCode6XNN(vx, value)
    case OpCode(0x7, vx, _, _, _, value) => opCode7XNN(vx, value)
    case OpCode(0x8, vx, vy, 0x0, _, _)  => opCode8XY0(vx, vy)
    case OpCode(0x8, vx, vy, 0x1, _, _)  => opCode8XY1(vx, vy)
    case OpCode(0x8, vx, vy, 0x2, _, _)  => opCode8XY2(vx, vy)
    case OpCode(0x8, vx, vy, 0x3, _, _)  => opCode8XY3(vx, vy)
    case OpCode(0x8, vx, vy, 0x4, _, _)  => opCode8XY4(vx, vy)
    case OpCode(0x8, vx, vy, 0x5, _, _)  => opCode8XY5(vx, vy)
    case OpCode(0x8, vx, vy, 0x6, _, _)  => opCode8XY6(vx, vy)
    case OpCode(0x8, vx, vy, 0x7, _, _)  => opCode8XY7(vx, vy)
    case OpCode(0x8, vx, vy, 0xE, _, _)  => opCode8XYE(vx, vy)
    case OpCode(0x9, vx, vy, 0x0, _, _)  => opCode9XY0(vx, vy)
    case OpCode(0xA, _, _, _, addr, _)   => opCodeANNN(addr)
    case OpCode(0xB, _, _, _, addr, _)   => opCodeBNNN(addr)
    case OpCode(0xC, vx, _, _, _, value) => opCodeCXNN(vx, value)
    case OpCode(0xD, vx, vy, h, _, _)    => opCodeDXYN(vx, vy, h)
    case OpCode(0xE, vx, 0x9, 0xE, _, _) => opCodeEX9E(vx)
    case OpCode(0xE, vx, 0xA, 0x1, _, _) => opCodeEXA1(vx)
    case OpCode(0xF, vx, 0x0, 0x7, _, _) => opCodeFX07(vx)
    case OpCode(0xF, vx, 0x1, 0x5, _, _) => opCodeFX15(vx)
    case OpCode(0xF, vx, 0x1, 0xE, _, _) => opCodeFX1E(vx)
    case OpCode(0xF, vx, 0x5, 0x5, _, _) => opCodeFX55(vx)
    case OpCode(0xF, vx, 0x6, 0x5, _, _) => opCodeFX65(vx)
    case _: OpCode                       => println(f"OpCode not implemented")
  }

  // 00E0: Clears the screen.
  private def opCode00E0(): Unit = graphicMemory.clear()

  // 00EE: Returns from a subroutine.
  def opCode00EE(): Unit = {
    programCounter = stack.head
    stack = stack.tail
  }

  // 1NNN: Jumps to address NNN.
  private def opCode1NNN(addr: Short): Unit = programCounter = addr

  // 2NNN: Calls subroutine at NNN.
  def opCode2NNN(addr: Short): Unit = {
    stack = programCounter :: stack
    programCounter = addr
  }

  // 3XNN: Skips the next instruction if VX equals NN.
  def opCode3XNN(vx: Byte, value: UByte): Unit = if (registers(vx) == value) advanceProgramCounter()

  // 4XNN: Skips the next instruction if VX doesn't equal NN.
  def opCode4XNN(vx: Byte, value: UByte): Unit = if (registers(vx) != value) advanceProgramCounter()

  // 5XY0: Skips the next instruction if VX equals VY.
  def opCode5XY0(vx: Byte, vy: Byte): Unit = if (registers(vx) == registers(vy)) advanceProgramCounter()

  // 6XNN: Sets VX to NN.
  def opCode6XNN(vx: Byte, value: UByte): Unit = registers(vx) = value

  // 7XNN: Adds NN to VX.
  def opCode7XNN(vx: Byte, value: UByte): Unit = registers(vx) = registers(vx) + value

  // 8XY0: Sets VX to the value of VY.
  def opCode8XY0(vx: Byte, vy: Byte): Unit = registers(vx) = registers(vy)

  // 8XY1: Sets VX to VX | VY.
  def opCode8XY1(vx: Byte, vy: Byte): Unit = registers(vx) = registers(vx) | registers(vy)

  // 8XY2: Sets VX to VX & VY.
  def opCode8XY2(vx: Byte, vy: Byte): Unit = registers(vx) = registers(vx) & registers(vy)

  // 8XY3: Sets VX to VX ^ VY.
  def opCode8XY3(vx: Byte, vy: Byte): Unit = registers(vx) = registers(vx) ^ registers(vy)

  // 8XY4: Adds VY to VX. VF is set to 1 when there's a carry, and to 0 when there isn't.
  def opCode8XY4(vx: Byte, vy: Byte): Unit = {
    val result = registers(vx) + registers(vy)

    // TODO: This feels terrible. There has to be a better way.
    if ((0x00FF & registers(vx).value) + (0x0FF & registers(vy).value) > 0xFF) {
      registers(0xF) = UByte(1)
    } else {
      registers(0xF) = UByte(0)
    }

    registers(vx) = result
  }

  // 8XY5: VY is subtracted from VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
  def opCode8XY5(vx: Byte, vy: Byte): Unit = {
    registers(0xF) = if (registers(vx) > registers(vy)) UByte(1) else UByte(0)
    registers(vx) = registers(vx) - registers(vy)
  }

  // 8XY6: Stores the least significant bit of VX in VF and then shifts VX to the right by 1.
  def opCode8XY6(vx: Byte, vy: Byte): Unit = {
    registers(0xF) = registers(vx) & UByte(0x0001)
    registers(vx) = registers(vx) >>> 1
  }

  // 8XY7: Sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
  def opCode8XY7(vx: Byte, vy: Byte): Unit = {
    registers(0xF) = if (registers(vy) > registers(vx)) UByte(1) else UByte(0)
    registers(vx) = registers(vy) - registers(vx)
  }

  // 8XYE: Stores the most significant bit of VX in VF and then shifts VX to the left by 1.
  def opCode8XYE(vx: Byte, vy: Byte): Unit = {
    registers(0xF) = (registers(vx) & UByte(0x80)) >>> 7
    registers(vx) = registers(vx) << 1
  }

  // 9XY0: Skips the next instruction if VX doesn't equal VY.
  def opCode9XY0(vx: Byte, vy: Byte): Unit = if (registers(vx) != registers(vy)) advanceProgramCounter()

  // ANNN: Sets I to the address NNN.
  def opCodeANNN(address: Short): Unit = addressI = address

  // BNNN: Jumps to the address NNN plus V0.
  def opCodeBNNN(address: Short): Unit = programCounter = (registers(0).value + address).toShort

  // CXNN: Sets VX to the result of a bitwise and operation on a random number (Typically: 0 to 255) and NN.
  // TODO: Untested
  def opCodeCXNN(vx: Byte, value: UByte): Unit = registers(vx) = UByte(Random.nextInt(256) & value.value)

  // DXYN: Draws a sprite at coordinate (VX, VY) that has a width of 8 pixels and a height of N pixels.
  def opCodeDXYN(vx: Byte, vy: Byte, height: Byte): Unit = {
    val x = Math.max(0, registers(vx).value)
    val y = Math.max(0, registers(vy).value)

    registers(0xF) = UByte(0)

    var offsetY = 0
    for (dy <- y until (y + height)) {
      val newValue = memory.get(addressI + offsetY).value
      offsetY += 1

      for (dx <- 0 until 8) {
        val wrappedX = (x + dx) % 64
        val wrappedY = dy       % 32

        val currentValue = graphicMemory.get(wrappedX, wrappedY)
        val bit          = if ((((newValue << dx) & 0x80) >>> 7) == 0) false else true

        registers(0xF) = if (currentValue && !bit) UByte(1) else registers(0xF)
        graphicMemory.set(wrappedX, wrappedY, currentValue ^ bit)
      }
    }
  }

  // EX9E: Skips the next instruction if the key stored in VX is pressed.
  def opCodeEX9E(vx: Byte): Unit = {
    val key = registers(vx)
    if (keyDispatcher.isPressed(key.value)) advanceProgramCounter()
  }

  // EXA1: Skips the next instruction if the key stored in VX is not pressed.
  def opCodeEXA1(vx: Byte): Unit = {
    val key = registers(vx)
    if (!keyDispatcher.isPressed(key.value)) advanceProgramCounter()
  }

  // FX07: Sets VX to the value of the delay timer.
  def opCodeFX07(vx: Byte): Unit = registers(vx) = delayTimer

  // TODO FX0A: A key press is awaited, and then stored in VX. (Blocking Operation. All instruction halted until next key event)

  // FX15: Sets the delay timer to VX.
  def opCodeFX15(vx: Byte): Unit = delayTimer = registers(vx)

  // TODO FX18: Sets the sound timer to VX.

  // FX1E: Adds VX to I.
  def opCodeFX1E(vx: Byte): Unit = addressI = (addressI + registers(vx).value).toShort

  // TODO FX29: Sets I to the location of the sprite for the character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font.
  // TODO FX33: Stores the binary-coded decimal representation of VX.

  // FX55: Stores V0 to VX (including VX) in memory starting at address I. The offset from I is increased by 1 for each value written, but I itself is left unmodified.
  def opCodeFX55(vx: Byte): Unit = for (registerIndex <- 0 to vx) {
    memory.set(addressI + registerIndex, registers(registerIndex))
  }

  // FX65: Fills V0 to VX (including VX) with values from memory starting at address I. The offset from I is increased by 1 for each value written, but I itself is left unmodified.
  def opCodeFX65(vx: Byte): Unit = for (registerIndex <- 0 to vx) {
    registers(registerIndex) = memory.get(addressI + registerIndex)
  }
}
