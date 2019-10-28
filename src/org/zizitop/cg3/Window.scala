package org.zizitop.cg3

import java.awt.event.{KeyAdapter, KeyListener, MouseAdapter, MouseEvent, MouseListener, MouseMotionAdapter, MouseMotionListener}
import java.awt.image.{BufferedImage, DataBufferInt}
import java.awt.{Canvas, Color, Graphics2D}

import javax.swing.{JFrame, WindowConstants}

class Window(val wwidth: Int, val wheight: Int, val wtitle: String,
             val wmouseListener: MouseListener = new MouseAdapter {},
             val wmmouseListener: MouseMotionListener = new MouseMotionAdapter {},
             val wkeyListener: KeyListener = new KeyAdapter {}) extends JFrame(wtitle) {
  private val screenIm = new BufferedImage(wwidth, wheight, BufferedImage.TYPE_INT_RGB)
  private val screen = screenIm.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData()
  private val canvas = new WCanvas

  var rotation = 0
  var scale = 1.0
  var translate = 0

  setSize(wwidth + 5, wheight + 35)
  getContentPane.add(canvas)

  canvas.addMouseListener(wmouseListener)
  canvas.addKeyListener(wkeyListener)
  canvas.addMouseMotionListener(wmmouseListener)

  setLocationRelativeTo(null)
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  setVisible(true)

  private class WCanvas extends Canvas {
    setSize(wwidth, wheight)
    def draw() {
      val bs = getBufferStrategy

      if(bs == null) {
        //Double fuffering
        this.createBufferStrategy(2)
        return
      }

      try {
        val gg = bs.getDrawGraphics().asInstanceOf[Graphics2D]

        gg setColor Color.BLACK

        gg fillRect(0, 0, getWidth(), getHeight())

        gg.translate(getWidth/2, getHeight/2)
        gg scale (scale, scale)
        gg rotate (Math.toRadians(rotation))
        gg.drawImage(screenIm, -getWidth/2 + translate, -getHeight/2 + translate, null)
        gg scale (1.0/scale, 1.0/scale)
        gg translate (-getWidth/2, -getHeight/2)

        gg.dispose()
        bs.show()
      } catch {
        case e: IllegalStateException => e.printStackTrace()
      }
    }
  }

  def getBitmap(): Array[Int] = screen

  def setPixel(x: Long, y: Long, pixel: Int): Unit = screen((((x % wwidth) +wwidth) % wwidth).toInt +
    (((y % wheight) + wheight) % wheight).toInt * wwidth) = pixel
  def getPixel(x: Long, y: Long): Int = screen(x.toInt + y.toInt * wwidth)

  def redraw(): Unit = {
    canvas.draw()
  }
}
