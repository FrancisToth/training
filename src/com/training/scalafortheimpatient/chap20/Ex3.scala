package com.training.scalafortheimpatient.chap20

import java.io.File

import akka.actor._

import scala.io.Source.fromFile
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

object Ex3 extends App {

  case class FINDWORDS(path: String, regex: Regex)
  case class BROWSE(file: File, regex: Regex)
  case class PARSEFILE(file: File, regex: Regex)
  case class WORDFOUND(words: Seq[String])

  val system = ActorSystem("Hello")
  private val of: ActorRef = system.actorOf(Props[Master], "master")
  of ! new FINDWORDS("/Users/francistoth/Desktop/test", "test".r)

  class Master extends Actor {
    override def receive: Receive = {
      case FINDWORDS(path, regex) =>
        context.become(run())
        context.actorOf(Props(new Browser), s"root_browser") ! BROWSE(new File(path), regex)
    }

    def run(totalWord: Int = 0): Receive = {
      case WORDFOUND(words) =>
//        if(context.children.toSeq.nonEmpty) {
//          context.become(run(totalWord + words.length))
//        } else {
          println(words.length)
          context.stop(self)
          context.system.shutdown()
//        }
    }
  }

  class Browser(id: Int = 0) extends Actor {
    override def receive: Receive = {
      case msg =>
        context.become(run())
        self.forward(msg)
    }

    def run(childCount: Int = 0, wordsFound: Seq[String] = Seq()): Receive = {
      case BROWSE(file, regex) =>
        if (file.isDirectory) {
          val children = file.listFiles
          context.become(run(children.length))
          file.listFiles.foreach { child =>
            context.actorOf(Props(new Browser(id)), s"b_${java.util.UUID.randomUUID.toString}") ! BROWSE(child, regex)
          }
        } else {
          context.become(run(1))
          context.actorOf(Props(new FileParser), s"fp_${java.util.UUID.randomUUID.toString}") ! PARSEFILE(file, regex)
        }
      case WORDFOUND(words) =>
        if(childCount == 1) {
          context.parent ! WORDFOUND(words)
          context.stop(self)
        } else {
          context.become(run(childCount - 1, wordsFound ++ words))
        }
    }
  }

  class FileParser extends Actor {
    override def receive: Receive = {
      case PARSEFILE(file, regex) =>
        println(s"parsing ${file.getCanonicalPath}")
        val words = Try(fromFile(file).getLines()
              .flatMap(_.split("\\s|\n"))
              .filter(regex.findFirstIn(_).isDefined)
        ).getOrElse(Seq()).toSeq

        sender ! WORDFOUND(words)
        context.stop(self)
    }
  }
}
