package com.training.scalafortheimpatient.chap20

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Status}

import scala.util.{Failure, Success}

//object Ex1 extends App {
//
//  val system = ActorSystem("Hello")
//  private val numbers: Seq[Double] = Seq(1, 2, 3, 4, 5)
//  println(s"should be ${numbers.sum / numbers.length}")
//  private val of: ActorRef = system.actorOf(Props[Dispatcher], "master")
//  of ! new AVG(numbers)
//
//  case class RESULT(avg: Double)
//
//  case class AVG(numbers: Seq[Double], length: Int) {
//    def this(numbers: Seq[Double]) = this(numbers, numbers.length)
//  }
//
//  class Dispatcher extends Actor {
//
//    override def receive: Receive = {
//      case AVG(numbers, length) =>
//        val groups = numbers.grouped(3).toSeq
//        context.become(working(groups.length, 0))
//        groups.zipWithIndex.foreach { slice =>
//          context.actorOf(Props(new Worker()), s"w1${slice._2}") ! AVG(slice._1, length)
//        }
//    }
//
//    def working(n: Int, total: Double): Receive = {
//      case RESULT(avg) =>
//        context.stop(sender)
//        if (n <= 1) {
//          println(total)
//          context.stop(self)
//          context.system.shutdown()
//        } else {
//          context.become(working(n - 1, total + avg))
//        }
//    }
//  }
//
//  class Worker extends Actor {
//    override def receive = {
//      case AVG(numbers, length) =>
//        sender ! RESULT(numbers.sum / length)
//    }
//  }
//}
//
//object ActorLab extends App {
//  import akka.actor.ActorRef
//  import akka.pattern.ask
//  import scala.concurrent.duration._
//
//  implicit val timeout = akka.util.Timeout(1 second)
//  import scala.concurrent.ExecutionContext.Implicits.global
//
//  case class GET(url: String)
//  case class RESPONSE(content: String)
////  case class CACHE(value: String)
//
////  case class CONTENTNOTFOUND(url: String)
////  case class CONTENTFOUND(content: String)
//
////  system.actorSelection(s"akka.tcp:// akkademy@$remoteAddress/user/akkademy-db")
//
//  val system = ActorSystem("Local")
//  val cacheActor = system.actorOf(Props(Cache(Map("http://www.coding-hipster.com" -> "Youpi !"))))
//  val httpServer: ActorRef = system.actorOf(Props(HttpServer(cacheActor)), "httpServer")
//
//  (httpServer ? new GET("http://www.coding-hipster.com")).onComplete {
//    case Success(response: String) => println(response); system.shutdown();
//    case Failure(ex) => println(s"Error !$ex"); system.shutdown();
//  }
//
//  case class HttpServer(cacheActor: ActorRef) extends Actor {
//    override def receive: Receive = {
//      case GET(url) =>
//        val senderRef = sender()
//        (cacheActor ? GET(url)).onComplete {
//          case Success(response: String) => senderRef ! response
//          case Failure(ex) => senderRef ! s"404 - $url"
//        }
//    }
//  }
//
//  case class Cache(contents: Map[String, String] = Map.empty) extends Actor {
//    override def receive: Receive = {
//      case GET(url) => contents.get(url) match {
//        case Some(content) => sender() ! content
//        case _ => sender() ! Status.Failure(new RuntimeException(s"not found - $url"))
//      }
//    }
//  }
//}

object ActorLab2 extends App {
  import akka.actor.ActorRef
  import akka.pattern.ask
  import scala.concurrent.duration._

  implicit val timeout = akka.util.Timeout(1 second)

  case class Get(url: String)
  case class Response(content: String)
  //  case class CACHE(value: String)

    case class ContentFetchedFromHTTP(content: String)
    case class Http404(url: String)
    case class ContentNotFoundInCache(url: String)
    case class ContentFoundInCache(content: String)

  //  system.actorSelection(s"akka.tcp:// akkademy@$remoteAddress/user/akkademy-db")

  val system = ActorSystem("Local")

  val cacheActor = system.actorOf(Props(Cache(Map("http://www.coding-hipster.com" -> "Youpi !"))))
  val httpServer: ActorRef = system.actorOf(Props[HttpServer], "httpServer")

  val client = system.actorOf(Props(Client(httpServer, cacheActor)))

  client ! new Get("test")
//  context.system.scheduler.scheduleOnce(timeout.duration, extraActor, "timeout")

  case class Client(httpServerActor: ActorRef, cacheActor: ActorRef) extends Actor {
    override def receive: Receive = {
      case Get(url) => cacheActor ! Get(url)
      case ContentFoundInCache(content) => println(s"cache : $content")
      case ContentNotFoundInCache(url) => httpServerActor ! Get(url)
      case ContentFetchedFromHTTP(content) => println(s"http : $content")
      case Http404(url) => println(s"404 - $url")
    }
  }

//  case class Logger(delegate: ActorRef) extends Actor {
//    override def receive: Receive = {
//      case Get(url) => println(s"Get : $url"); delegate.forward(Get(url))
//      case ContentFoundInCache(content) => println(s"cache : $content")
//      case ContentNotFoundInCache(url) => httpServerActor ! Get(url)
//      case ContentFetchedFromHTTP(content) => println(s"http : $content")
//      case Http404(url) => println(s"404 - $url")
//    }
//  }

  class HttpServer extends Actor {
    override def receive: Receive = {
      case Get(url) =>
        if(url == "test") sender() ! ContentFetchedFromHTTP("200 - Youpi !")
        else sender() ! Http404(url)
    }
  }

  case class Cache(contents: Map[String, String] = Map.empty) extends Actor {
    override def receive: Receive = {
      case Get(url) => contents.get(url) match {
        case Some(content) => sender() ! ContentFoundInCache(content)
        case None => sender() ! ContentNotFoundInCache(url)
      }
    }
  }
}