package org.zizitop.cg3

import javax.imageio.ImageIO
import java.awt.{Color, Point}
import java.awt.event.{KeyAdapter, KeyEvent, MouseAdapter, MouseEvent}
import java.awt.image.BufferedImage
import java.io.IOException
import java.util

object Main extends MouseAdapter {
  val window = new Window(640, 480, "Lab 2", this, this, KeyListener)

  val fillThreads = new util.ArrayList[Thread]()

  def fillSquare0(window: Window, point: Point, newColor: Int): Unit = {
    fillThreads.add(Thread.currentThread())
    var oldColor = window.getPixel(point.x, point.y)
    if(newColor == oldColor) return; //avoid infinite loop

    val stack = new util.Stack[Point]
    stack.push(new Point(point.x, point.y));
    while(!stack.empty()) {
      val pp = stack.pop()
      Thread.sleep(1)
      window.setPixel(pp.x, pp.y, newColor)
      for(dx <- -1 to 1; dy <- -1 to 1; if (dx != dy && dx * dy == 0)) {
        val xx = pp.x + dx
        val yy = pp.y + dy
        if(xx >= 0 && xx < window.wwidth && yy >= 0 && yy < window.wheight && window.getPixel(xx, yy) == oldColor) {
          stack.push(new Point(xx, yy))
        }
      }
    }
    fillThreads.remove(Thread.currentThread())
  }

  def fillSquare1(window: Window, point: Point, newColor: Int): Unit = {
    fillThreads.add(Thread.currentThread())
    var x = point.x
    var y = point.y
    var oldColor = window.getPixel(x, y)

    if(oldColor == newColor) return;

    var x1 = 0;
    var spanAbove, spanBelow :Boolean = false;

    val stack = new util.Stack[Point]
    stack.push(new Point(x, y));
    while(!stack.empty()) {
      Thread.sleep(5)
      var pp = stack.pop()
      x = pp.x
      y = pp.y

      x1 = x;

      while(x1 >= 0 && window.getPixel(x1, y) == oldColor) x1 -= 1;
      x1 += 1;
      spanAbove = false
      spanBelow = false

      while(x1 < window.wwidth && window.getPixel(x1, y) == oldColor) {
        window.setPixel(x1, y, newColor);
        if(!spanAbove && y > 0 && window.getPixel(x1, y - 1) == oldColor) {
          stack.push(new Point(x1, y - 1))
          spanAbove = true
        } else if(spanAbove && y > 0 && window.getPixel(x1, y - 1) != oldColor) {
          spanAbove = false
        }

        if(!spanBelow && y < window.wheight - 1 && window.getPixel(x1, y + 1) == oldColor) {
          stack.push(new Point(x1, y + 1))
          spanBelow = true
        } else if(spanBelow && y < window.wheight - 1 && window.getPixel(x1, y + 1) != oldColor) {
          spanBelow = false;
        }

        x1 += 1;
      }
    }
    fillThreads.remove(Thread.currentThread())
  }

  def drawBezierLine(mesh: Array[(Double, Double)], drawColor: Int): Unit = {
    val points = Array.ofDim[(Double, Double)](mesh.length, mesh.length)
    System.arraycopy(mesh, 0, points(0), 0, mesh.length)

    var previousPoint = mesh(0)

    val step = 0.01f
    var t = 0.0f
    while (t < 1.0f) {
      val n = mesh.length
      var i = 1
      while (i < mesh.length) {
        var j = 0

        while (j < n - i){
          points(i)(j) = newPoint(points(i - 1)(j), points(i - 1)(j + 1), t)
          j += 1
        }

        i += 1
      }
      //  putPixel(points[mesh.length-1][0], drawColor);
      putLine(previousPoint, points(mesh.length - 1)(0), drawColor)
      previousPoint = points(mesh.length - 1)(0)

      t += step
    }
  }

  private def newPoint(a: (Double, Double), b: (Double, Double), t: Double) =
    new (Double, Double)(a._1 * (1.0f - t) + b._1 * t, a._2 * (1.0f - t) + b._2 * t)

  private def putLine(a: (Double, Double), b: (Double, Double), drawColor: Int): Unit = {
    drawLine(window, new Point(a._1.toInt, a._2.toInt), new Point(b._1.toInt, b._2.toInt))
  }

  import Math._

  def plot(window: Window, x: Long, y: Long, double: Double): Unit = {
    var a = (double * 256).toInt
    if (a > 255) a = 255
    if(a < 0) a = 0
    window.setPixel(x, y, a | (a << 8) | (a << 16))
  }

  def drawLine(window: Window, p1: Point, p2: Point): Unit = {
    var x,y,dx,dy,incx,incy,pdx,pdy,es,el,err = 0

    dx = p2.x - p1.x
    dy = p2.y - p1.y

    incx = Math.signum(dx).toInt
    incy = Math.signum(dy).toInt

    if (dx < 0) dx = -dx
    if (dy < 0) dy = -dy

    if (dx > dy) {
      pdx = incx
      pdy = 0
      es = dy
      el = dx
    } else {
      pdx = 0
      pdy = incy
      es = dx
      el = dy
    }

    x = p1.x
    y = p1.y
    err = el / 2
    window.setPixel(x, y, 0xffffff)

    var t = 0
    while (t < el) {
      err -= es

      if (err < 0) {
        err += el
        x += incx
        y += incy
      } else {
        x += pdx
        y += pdy
      }

      window.setPixel(x, y, 0xffffff)

      t += 1
    }
  }

  private val pointsList = new util.ArrayList[(Double, Double)]

  private var mousePos = new Point(0, 0)

  override def mouseDragged(e: MouseEvent): Unit = mousePos = e.getPoint
  override def mouseMoved(e: MouseEvent): Unit = mousePos = e.getPoint

  override def mouseReleased(e: MouseEvent): Unit = {
    e.getButton match {
      case MouseEvent.BUTTON1 =>
        pointsList add((e.getX, e.getY))
        for(x <- e.getX - 1 to e.getX + 1; y <- e.getY - 1 to e.getY + 1) {
          window.setPixel(x, y, 0xff00)
        }
      case MouseEvent.BUTTON3 =>
        if(pointsList.size() > 1) {
          drawBezierLine(pointsList.toArray(new Array[(Double, Double)](pointsList.size())), -1)
          pointsList.clear()
        }
    }
  }

  object KeyListener extends KeyAdapter {
    override def keyReleased(e: KeyEvent): Unit = {
      e.getKeyCode match {
        case KeyEvent.VK_LEFT => window.rotation -= 45
        case KeyEvent.VK_RIGHT => window.rotation += 45
        case KeyEvent.VK_UP => window.scale /= 2.0
        case KeyEvent.VK_DOWN => window.scale *= 2.0
        case KeyEvent.VK_A => window.translate += 50
        case KeyEvent.VK_D => window.translate -= 50
        case KeyEvent.VK_C =>
          fillThreads.forEach((t) => t.stop());
          fillThreads.clear()
          for(i <- window.getBitmap().indices) window.getBitmap()(i) = 0
          pointsList.clear()
        case KeyEvent.VK_M =>
          for(x <- 0 until window.wwidth/2; y <- 0 until window.wheight) {
            val p1 = window.getPixel(x, y)
            val p2 = window.getPixel(window.wwidth - 1 - x, y)
            window.setPixel(x, y, p2)
            window.setPixel(window.wwidth - 1 - x, y, p1)
          }
        case KeyEvent.VK_1 => new Thread(() => fillSquare0(window, mousePos, 0x0000ff)).start();
        case KeyEvent.VK_2 => new Thread(() => fillSquare1(window, mousePos, 0x0000ff)).start();
        case _ =>print(e.getKeyCode)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    while(true) {
      Thread.sleep(50)
      window.redraw()
    }
  }
}
