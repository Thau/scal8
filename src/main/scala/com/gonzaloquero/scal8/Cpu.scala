package com.gonzaloquero.scal8

import scala.util.Random

class Cpu(memory: Memory, graphicMemory: GraphicMemory, keyDispatcher: KeyDispatcher) {
  var addressI: Short         = 0
  var programCounter: Short   = 0x200
  var stack: List[Short]      = List()
  val registers: Array[UByte] = new Array[UByte](16)
  var delayTimer: UByte       = UByte.of(0)

  def tick(): Unit = {
    val opCode = getNextOpCode
    executeOpCode(opCode)

  }

  def clockTick(): Unit = {
    if (delayTimer > UByte.of(0)) {
      delayTimer = delayTimer - UByte.of(1)
    }
  }

  private def getNextOpCode: Short = {
    val firstPart  = memory.get(programCounter).getValue << 8
    val secondPart = memory.get(programCounter + 1).getValue & 0x00FF

    advanceProgramCounter()
    ((firstPart | secondPart).toShort & 0x0000FFFF).toShort
  }

  private def advanceProgramCounter(): Unit = {
    programCounter = (programCounter + 2).toShort
  }

  private def getOpCodeAddressValue(opCode: Short) = {
    (opCode & 0x0FFF).toShort
  }

  private def getOpCodeXRegisterNumber(opCode: Short): Byte = {
    ((opCode & 0x0F00) >>> 8).toByte
  }

  private def getOpCodeYRegisterNumber(opCode: Short): Byte = {
    ((opCode & 0x00F0) >>> 4).toByte
  }

  private def getOpCodeValue(opCode: Short): UByte = {
    UByte.of((opCode & 0x00FF).toByte)
  }

  private def executeOpCode(opCode: Short): Unit = opCode & 0xF000 match {
    case 0x0000 => opCode0XXX(opCode)
    case 0x1000 => opCode1NNN(opCode)
    case 0x2000 => opCode2NNN(opCode)
    case 0x3000 => opCode3XNN(opCode)
    case 0x4000 => opCode4XNN(opCode)
    case 0x5000 => opCode5XY0(opCode)
    case 0x6000 => opCode6XNN(opCode)
    case 0x7000 => opCode7XNN(opCode)
    case 0x8000 => opCode8XYN(opCode)
    case 0x9000 => opCode9XY0(opCode)
    case 0xA000 => opCodeANNN(opCode)
    case 0xB000 => opCodeBNNN(opCode)
    case 0xC000 => opCodeCXNN(opCode)
    case 0xD000 => opCodeDXYN(opCode)
    case 0xE000 => opCodeEXNN(opCode)
    case 0xF000 => opCodeFXNN(opCode)
    case _      => println(f"OpCode $opCode%x not implemented")
  }

  private def opCode0XXX(opCode: Short): Unit = opCode match {
    case 0x00E0 => opCode00E0()
    case 0x00EE => opCode00EE()
    case 0x0000 => () // Noop
    case _      => println(f"OpCode 0XXX $opCode%x not implemented")
  }

  // 00E0: Clears the screen.
  private def opCode00E0(): Unit = {
    graphicMemory.clear()
  }

  // 00EE: Returns from a subroutine.
  def opCode00EE(): Unit = {
    programCounter = stack.head
    stack = stack.tail
  }

  // 1NNN: Jumps to address NNN.
  private def opCode1NNN(opCode: Short): Unit = {
    programCounter = getOpCodeAddressValue(opCode)
  }

  // 2NNN: Calls subroutine at NNN.
  def opCode2NNN(opCode: Short): Unit = {
    stack = programCounter :: stack
    programCounter = getOpCodeAddressValue(opCode)
  }

  // 3XNN: Skips the next instruction if VX equals NN.
  def opCode3XNN(opCode: Short): Unit = {
    val registerNumber = getOpCodeXRegisterNumber(opCode)
    val value          = getOpCodeValue(opCode)

    if (registers(registerNumber) == value) {
      advanceProgramCounter()
    }
  }

  // 4XNN: Skips the next instruction if VX doesn't equal NN.
  def opCode4XNN(opCode: Short): Unit = {
    val registerNumber = getOpCodeXRegisterNumber(opCode)
    val value          = getOpCodeValue(opCode)

    if (registers(registerNumber) != value) {
      advanceProgramCounter()
    }
  }

  // 5XY0: Skips the next instruction if VX equals VY.
  def opCode5XY0(opCode: Short): Unit = {
    val registerXNumber = getOpCodeXRegisterNumber(opCode)
    val registerYNumber = getOpCodeYRegisterNumber(opCode)

    if (registers(registerXNumber) == registers(registerYNumber)) {
      advanceProgramCounter()
    }
  }

  // 6XNN: Sets VX to NN.
  def opCode6XNN(opCode: Short): Unit = {
    val registerNumber = getOpCodeXRegisterNumber(opCode)
    val value          = getOpCodeValue(opCode)

    registers(registerNumber) = value
  }

  // 7XNN: Adds NN to VX.
  def opCode7XNN(opCode: Short): Unit = {
    val registerNumber = getOpCodeXRegisterNumber(opCode)
    val value          = getOpCodeValue(opCode)

    registers(registerNumber) = registers(registerNumber) + value
  }

  // 8XYN: Bit and math operations based on N
  def opCode8XYN(opCode: Short): Unit = {
    val registerXNumber = getOpCodeXRegisterNumber(opCode)
    val registerYNumber = getOpCodeYRegisterNumber(opCode)
    val registerXValue  = registers(registerXNumber)
    val registerYValue  = registers(registerYNumber)

    registers(registerXNumber) = opCode & 0x000F match {
      // 8XY0: Sets VX to the value of VY.
      case 0x0000 => registerYValue
      // 8XY1: Sets VX to VX | VY.
      case 0x0001 => registerXValue | registerYValue
      // 8XY2: Sets VX to VX & VY.
      case 0x0002 => registerXValue & registerYValue
      // 8XY3: Sets VX to VX xor VY.
      case 0x0003 => registerXValue ^ registerYValue
      // 8XY4: Adds VY to VX. VF is set to 1 when there's a carry, and to 0 when there isn't.
      case 0x0004 =>
        val result = registerXValue + registerYValue

        // TODO: This feels terrible. There has to be a better way.
        if ((0x00FF & registerXValue.getValue) + (0x0FF & registerYValue.getValue) > 0xFF) {
          registers(0xF) = UByte.of(1)
        } else {
          registers(0xF) = UByte.of(0)
        }

        result
      // 8XY5: VY is subtracted from VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
      case 0x0005 =>
        registers(0xF) = if (registerXValue > registerYValue) {
          UByte.of(1)
        } else {
          UByte.of(0)
        }

        registerXValue - registerYValue
      // 8XY6: Stores the least significant bit of VX in VF and then shifts VX to the right by 1.
      case 0x0006 =>
        registers(0xF) = registerXValue & UByte.of(0x0001)
        registerXValue >>> 1
      // 8XY7: Sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
      case 0x0007 =>
        registers(0xF) = if (registerYValue > registerXValue) {
          UByte.of(1)
        } else {
          UByte.of(0)
        }

        registerYValue - registerXValue
      // 8XYE: Stores the most significant bit of VX in VF and then shifts VX to the left by 1.
      case 0x000E =>
        registers(0xF) = (registerXValue & UByte.of(0x80.toByte)) >>> 7
        registerXValue << 1
    }
  }

  // 9XY0: Skips the next instruction if VX doesn't equal VY.
  def opCode9XY0(opCode: Short): Unit = {
    val registerXNumber = getOpCodeXRegisterNumber(opCode)
    val registerYNumber = getOpCodeYRegisterNumber(opCode)

    if (registers(registerXNumber) != registers(registerYNumber)) {
      advanceProgramCounter()
    }
  }

  // ANNN: Sets I to the address NNN.
  def opCodeANNN(opCode: Short): Unit = {
    addressI = getOpCodeAddressValue(opCode)
  }

  // BNNN: Jumps to the address NNN plus V0.
  def opCodeBNNN(opCode: Short): Unit = {
    val address = getOpCodeAddressValue(opCode)
    programCounter = (registers(0).getValue + address).toShort
  }

  // CXNN: Sets VX to the result of a bitwise and operation on a random number (Typically: 0 to 255) and NN.
  // TODO: Untested
  def opCodeCXNN(opCode: Short): Unit = {
    val registerXNumber = getOpCodeXRegisterNumber(opCode)
    val value           = getOpCodeValue(opCode)

    registers(registerXNumber) = UByte.of((Random.nextInt(256) & value.getValue).toByte)
  }

  // DXYN: Draws a sprite at coordinate (VX, VY) that has a width of 8 pixels and a height of N pixels.
  def opCodeDXYN(opCode: Short): Unit = {
    val registerXNumber = getOpCodeXRegisterNumber(opCode)
    val registerYNumber = getOpCodeYRegisterNumber(opCode)
    val height          = opCode & 0x00F
    val x               = Math.max(0, registers(registerXNumber).getValue)
    val y               = Math.max(0, registers(registerYNumber).getValue)

    registers(0xF) = UByte.of(0)

    var offsetY = 0
    for (dy <- y until (y + height)) {

      val newValue = memory.get(addressI + offsetY).getValue
      offsetY += 1

      for (dx <- 0 until 8) {
        val wrappedX = (x + dx) % 64
        val wrappedY = dy       % 32

        val currentValue = graphicMemory.get(wrappedX, wrappedY)
        val bit          = if ((((newValue << dx) & 0x80) >>> 7) == 0) false else true

        registers(0xF) = if (currentValue && !bit) { UByte.of(1) } else { registers(0xF) }
        graphicMemory.set(wrappedX, wrappedY, currentValue ^ bit)
      }
    }
  }

  // EXNN: Keyboard operations
  def opCodeEXNN(opCode: Short): Unit = opCode & 0x00FF match {
    case 0x009E => opCodeEX9E(opCode)
    case 0x00A1 => opCodeEXA1(opCode)
  }

  // EX9E: Skips the next instruction if the key stored in VX is pressed.
  def opCodeEX9E(opCode: Short): Unit = {
    val registerNumber = getOpCodeXRegisterNumber(opCode)
    val key            = registers(registerNumber)

    if (keyDispatcher.isPressed(key.getValue)) {
      advanceProgramCounter()
    }
  }

  // EXA1: Skips the next instruction if the key stored in VX is not pressed.
  def opCodeEXA1(opCode: Short): Unit = {
    val registerNumber = getOpCodeXRegisterNumber(opCode)
    val key            = registers(registerNumber)

    if (!keyDispatcher.isPressed(key.getValue)) {
      advanceProgramCounter()
    }
  }

  // FXNN: Timer and memory operations.
  def opCodeFXNN(opCode: Short): Unit = {
    val registerNumber = getOpCodeXRegisterNumber(opCode)
    val value          = registers(registerNumber)
    val opCodeValue    = getOpCodeValue(opCode)

    opCodeValue.getValue match {
      // FX07: Sets VX to the value of the delay timer.
      case 0x0007 => registers(registerNumber) = delayTimer
      // TODO FX0A: A key press is awaited, and then stored in VX. (Blocking Operation. All instruction halted until next key event)
      // FX15: Sets the delay timer to VX.
      case 0x0015 => delayTimer = value
      // TODO FX18: Sets the sound timer to VX.
      // FX1E: Adds VX to I.
      case 0x001E => addressI = (addressI + value.getValue).toShort
      // TODO FX29: Sets I to the location of the sprite for the character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font.
      // TODO FX33: Stores the binary-coded decimal representation of VX.
      // FX55: Stores V0 to VX (including VX) in memory starting at address I. The offset from I is increased by 1 for each value written, but I itself is left unmodified.
      case 0x0055 =>
        for (registerIndex <- 0 to registerNumber) {
          memory.set(addressI + registerIndex, registers(registerIndex))
        }
      // FX65: Fills V0 to VX (including VX) with values from memory starting at address I. The offset from I is increased by 1 for each value written, but I itself is left unmodified.
      case 0x0065 =>
        for (registerIndex <- 0 to registerNumber) {
          registers(registerIndex) = memory.get(addressI + registerIndex)
        }
      case _ => println(f"OpCode $opCode%x not implemented")
    }
  }
}
