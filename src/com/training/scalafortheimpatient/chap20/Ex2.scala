package com.training.scalafortheimpatient.chap20

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import akka.actor.{Actor, Props, ActorRef, ActorSystem}

import scala.math._

object Ex2 extends App {
  // write a program that reads in a large image into a bufferedimage,
  // using javax.imageio.ImageIO.read. Use multiple actors,
  // each inverting the colors in a stripe of the image.
  // Once all inverted, print the result

  val system = ActorSystem("Hello")
  private val rawImage = javax.imageio.ImageIO.read(new File("/Users/francistoth/Desktop/hexagonnal 2.png"))
  private val of: ActorRef = system.actorOf(Props[Dispatcher], "master")
  of ! new PROCESS(rawImage)

  case class PROCESS(image: BufferedImage)
  case class INVERT(image: BufferedImage, startX: Int, endY:Int)
  case class RESULT(invertedSprite: BufferedImage, x: Int)

  class Dispatcher extends Actor {
    override def receive = {
      case PROCESS(image) =>
        context.become(working(ceil(image.getWidth / 50).toInt, image, Seq()))
        runActor(image, 0, 50)
    }

    def runActor(image: BufferedImage, start: Int, incr: Int): Unit = {
      val end = min(incr, image.getWidth - start)
      if(start < image.getWidth) {
        context.actorOf(Props(new Worker()), s"w_$start-$end") ! INVERT(image, start, end)
        runActor(image, start + incr, incr)
      }
    }

    def working(remainder: Int, rawImage: BufferedImage, stripes: Seq[(BufferedImage, Int)]): Receive = {
      case RESULT(invertedSprite, start) =>
        if(remainder == 0) {
          val graphics = rawImage.createGraphics
          stripes.foreach { stripeWithX =>
            val (stripe, x) = stripeWithX
            graphics.drawImage(stripe, null, x, 0)
          }
          graphics.dispose()
          ImageIO.write(rawImage, "png", new File("/Users/francistoth/Desktop/inverted.png"))
          context.stop(self)
          context.system.shutdown()
        } else {
          context.become(working(remainder - 1, rawImage, stripes :+ (invertedSprite, start)))
        }
    }
  }

  class Worker extends Actor {
    override def receive = {
      case INVERT(image, start, end) =>
        val tile = image.getSubimage(start, 0, end, image.getHeight)
        val coords = (0 to (end - 1)).flatMap { x => (0 to (image.getHeight - 1)).map(y => (x, y)) }

        coords.foreach { coord =>
          val (x, y) = coord
          val rgb = new Color(tile.getRGB(x, y), true)
          val invertedRgb = new Color(255 - rgb.getRed, 255 - rgb.getGreen, 255 - rgb.getBlue)
          tile.setRGB(x, y, invertedRgb.getRGB)
        }
        sender ! RESULT(tile, start)
        context.stop(self)
    }
  }
}