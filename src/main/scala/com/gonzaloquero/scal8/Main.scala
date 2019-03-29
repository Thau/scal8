package com.gonzaloquero.scal8

import java.io.{BufferedInputStream, FileInputStream}

import scalafx.Includes._
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.input.KeyEvent
import scalafx.scene.paint.Color

object Main extends JFXApp {
  val graphicMemory: GraphicMemory = new GraphicMemory
  val canvas                       = new Canvas(64 * 4, 32 * 4)

  val memory: Memory = loadRom(parameters.raw.head)
  val cpu: Cpu       = new Cpu(memory, graphicMemory, FXKeyDispatcher)

  new Thread {
    override def run(): Unit = {
      var delayT                = 0L
      var nextExpectedFrameTime = 0L

      while (true) {
        val frameTime = System.nanoTime()

        if (frameTime > nextExpectedFrameTime) {
          cpu.tick()

          nextExpectedFrameTime = frameTime + 2000000
        }

        delayT += System.nanoTime() - frameTime
        if (delayT >= 16000000) {
          cpu.clockTick()

          delayT = 0
        }
      }
    }

    start()
  }

  stage = new JFXApp.PrimaryStage {
    title.value = "Scal8"
    scene = new Scene(64 * 4, 32 * 4) {
      fill = Color.Black
      content = canvas
    }
    resizable = false

    show()

    handleEvent(KeyEvent.Any) { ke: KeyEvent =>
      FXKeyDispatcher.dispatchKeyEvent(ke)
    }

    AnimationTimer(_ => {
      render()
    }).start()
  }

  private def render(): Unit = {
    val dx = 4
    val dy = 4
    val gc = canvas.graphicsContext2D

    for {
      x <- 0 until graphicMemory.width
      y <- 0 until graphicMemory.height
      x1 = x * dx
      y1 = y * dy
      x2 = (x1 + 4) * dx
      y2 = (y1 + 4) * dy
    } {
      if (graphicMemory.get(x, y)) {
        gc.setFill(Color.White)
      } else {
        gc.setFill(Color.Black)
      }
      gc.fillRect(x1, y1, x2 - x1, y2 - y1)
    }
  }

  private def loadRom(romFileName: String): Memory = {
    val bis = new BufferedInputStream(new FileInputStream(romFileName))
    Memory.withProgram(
      Stream
        .continually(bis.read)
        .takeWhile(b => b != -1)
        .map(b => UByte.of(b.toByte))
    )
  }
}
