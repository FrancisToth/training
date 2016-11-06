package com.training.functionalprogramminginscala

import akka.actor.{Actor, ActorRef, ActorSystem, OneForOneStrategy, PoisonPill, Props, Terminated}
import akka.actor.Actor.Receive
import akka.actor.SupervisorStrategy.{Restart, Stop}
import com.training.functionalprogramminginscala.Domain._

import scala.util.Random

object chapter7 extends App {

  object Par {
    /* Par.map2 is a new higher-order function for combining the result of two parallel computations. What is its
       signature? Give the most general signature possible (don’t assume it works only for Int). */
    def map2() = {

    }
  }

  /* Before continuing, try to come up with representations for Par that make it possible to implement the functions of
     our API. */

  /* Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future. */

  /* def asyncF[A,B](f: A => B): A => Par[B] */

  /* Hard: Write this function, called sequence. No additional primitives are required. Do not call run. */
//  def sequence[A](ps: List[Par[A]]): Par[List[A]]

  /* Implement parFilter, which filters elements of a list in parallel. */
//  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]]*/

  /* Hard: Given map(y)(id) == y, it’s a free theorem that map(map(y)(g))(f) == map(y)(f compose g). (This is sometimes
     called map fusion, and it can be used as an optimization rather than spawning a separate parallel computation to
     compute the second mapping, we can fold it into the first mapping.) Can you prove it? You may want to read the
     paper “Theorems for Free!” (http://mng.bz/Z9f1) to better understand the “trick” of free theorems. */

  /* Hard: Take a look through the various static methods in Executors to get a feel for the different implementations
     of ExecutorService that exist. Then, before continuing, go back and revisit your implementation of fork and try to
     find a counterexample or convince yourself that the law holds for your implementation. */

  /* Hard: Show that any fixed-size thread pool can be made to deadlock given this imple- mentation of fork. */

  /* Hard: Our non-blocking representation doesn’t currently handle errors at all. If at any point our computation
     throws an exception, the run implementation’s latch never counts down and the exception is simply swallowed. Can you fix that?*/

  /* Implement choiceN and then choice in terms of choiceN. */

  /* There’s still something rather arbitrary about choiceN. The choice of List seems overly specific. Why does it
     matter what sort of container we have? For instance, what if, instead of a list of computations, we have a Map of
     them: */
//  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V]

  /* If you want, stop reading here and see if you can come up with a new and more general combinator in terms of which
     you can implement choice, choiceN, and choiceMap. */

  /* Implement this new primitive chooser, and then use it to implement choice and choiceN. */

  /* Implement join. Can you see how to implement flatMap using join? And can you implement join using flatMap? */
}

//trait ConfigGetter[T] { def get(propertyName: String)(implicit wrappedConfig: WrappedConfig): T }
//
//object ConfigGetter {
//  def config[T : ConfigGetter](propertyName: String)
//                              (implicit wrappedConfig: WrappedConfig): T =
//    implicitly[ConfigGetter[T]].get(propertyName)
//}
//
//trait WrappedConfig {
//  def getString(propertyName: String): String
//  def getInt(propertyName: String): Int
//  def getDouble(propertyName: String): Double
//
//
//  trait ListItemMapper[B] extends (ConfigItemType => B)
//
//  protected def list(propertyName: String): ConfigItemType
//
//
//}

//trait Configuration[T] { self =>
//
////  def config[A](propertyName: String): A = {
////    import self._
////    get(propertyName)
////  }
//
//  def config[A](propertyName: String): A = {
//    import self._
//    get(propertyName)
//  }
//
//  def get[A](propertyName: String)(implicit mapper: ConfValueMapper[T, A]): A
//}
//
//trait ConfValueMapper[-A, +B] extends (A => B)
//
//object Prod extends Configuration[String] {
//  override def get[A](propertyName: String)(implicit mapper: ConfValueMapper[String, A]): A = mapper("")
//
//  implicit val x = new ConfValueMapper[String, Int] {
//    override def apply(v1: String): Int = v1.toInt
//  }
//}
//
//object Test extends App {
//  println(Prod.config(""))
//}

//  type ListItemType
//  def getList[A](propertyName: String)(implicit mapper:ListItemMapper[A]): List[A]
//
//  trait ListItemMapper[+B] extends (ListItemType => B)
//
//  object ListItemMapper

//trait ListItemMapperBuilder[A] {
//  def build[B]()
//}

//object CollectionGetter {
//  implicit val seq = new ConfigGetter[Seq[Int]] {
//    def get(propertyName: String)(implicit conf: WrappedConfig): Seq[Int] = Seq(conf.getInt(propertyName), conf.getInt(propertyName))
//  }

//  implicit val intToString = new ListItemMapper[, String] {
//
//  }
//}

//object PairGetter {
//  implicit val pair = new ConfigGetter[(Int, String)] {
//    def get(propertyName: String)(implicit conf: WrappedConfig): (Int, String) = (conf.getInt("left"), conf.getString("right"))
//  }
//}
//
//object ProdConf extends WrappedConfig {
//  override def getString(propertyName: String): String = "1"
//  override def getDouble(propertyName: String): Double = 2.0
//  override def getInt(propertyName: String): Int = 3
//
//  override type ListItemType = Int
//
//  override def getList[A](propertyName: String)
//                         (implicit mapper: ListItemMapper[ListItemType, A]): List[A] = List(1).map(mapper)
//}
//
//object TestConf extends WrappedConfig {
//  override def getString(propertyName: String): String = "4"
//  override def getDouble(propertyName: String): Double = 5.0
//  override def getInt(propertyName: String): Int = 6
//
//  override type ListItemType = Int
//
//  override def getList[A](propertyName: String)
//                         (implicit mapper: ListItemMapper[ListItemType, A]): List[A] = List(2).map(mapper)
//}
//
//object TestApp extends App {
//  implicit val c = ProdConf
//  println(new Dummy().test.mkString("#"))
//}
//
//class Dummy {
//  import ConfigGetter._
//  import PairGetter._
//  import CollectionGetter._
//  def test(implicit conf: WrappedConfig) = config[Seq[Int]]("test")
//}

object Domain {
  case class Mssg1()
  case class Mssg2()
  case class Mssg3()
  case class Mssg4()
  case class Mssg5()

  case class Answer1()
  case class Answer2()
  case class Answer3()
  case class Answer4()

  case class Error()
  case class Success()

  case class Event1()
}

case class Parent(name: String) extends Actor {

  override val supervisorStrategy = OneForOneStrategy() {
    case e =>
      println("send default content back")
      Stop
  }


  override def receive: Receive = bidule()

  private def bidule(implicit watchedChildren: Int = 0): Receive = {
    case Mssg1() => watchAndCreate(Props(Child1(Answer1(), Answer2(), Mssg2())), name + "-child1") ! Mssg2()
    case Answer1() => watchAndCreate(Props(Child1(Answer2(), Answer2(), Mssg3())), name + "-child2") ! Mssg3()
    case Answer2() => watchAndCreate(Props(Child1(Answer3(), Answer3(), Mssg4())), name + "-child3") ! Mssg4()
    case Answer3() =>
      context.system.eventStream.publish(Event1())
      println("send content back")
      watchAndCreate(Props(Child1(Answer4(), Answer4(), Mssg5())), name + "-child4") ! Mssg5()
    case Answer4() =>
    case Terminated(a) =>
      context.unwatch(a)
      println("nb children : " + (watchedChildren - 1))
      if(watchedChildren - 1 <= 0)
        context.stop(self)
      else
        context.become(bidule(watchedChildren - 1))
  }


  def watchAndCreate(props: Props, name: String)(implicit aliveChildren: Int): ActorRef = {
    val ref = context.watch(context.actorOf(props, name))
    context.become(bidule(aliveChildren + 1))
    ref
  }

  override def postStop(): Unit = {
    super.postStop()
    println(s"Parent $name is stopping")
  }
}

case class Child1(answer:Any, answer2:Any, mssg: Any) extends Actor {
  override def receive: Receive = {
    case `mssg` =>
//      if(Random.nextBoolean()) throw new RuntimeException(s"Error from $self")
//      if(Mssg4() == mssg) throw new RuntimeException(s"Error from $self")
//      if(true) throw new RuntimeException(s"Error from $self")
      println(s"$mssg from $self")
      sender ! (if(Random.nextBoolean()) answer else answer2)
      context.stop(self)
  }

  override def postStop(): Unit = {
    println(s"$self is stopping")
    super.postStop()
  }
}

class Transverse extends Actor {

  override def postRestart(reason: Throwable): Unit = super.postRestart(reason)

  override def preStart(): Unit = {
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[Event1])
  }

  override def postStop(): Unit = {
    super.postStop()
    println("Transverse is stopping")
    context.system.eventStream.unsubscribe(self)
  }

  override def receive: Receive = {
    case Event1() => println("event 1 received")
  }
}

object TestActor extends App {
  val system = ActorSystem("on-spray-can")
  system.actorOf(Props[Transverse], "transverse")

  system.actorOf(Props(Parent("parent1")), "parent1") ! Mssg1()
//  system.actorOf(Props(Parent("parent2")), "parent2") ! Mssg1()
}

class TimeoutActor extends Actor {
  import akka.io.IO
  import akka.pattern.{ask, pipe}
  import scala.concurrent.Future
  import scala.concurrent.duration._
  //
  //       Future(1/0)
  override def receive: Receive = {
    case _ =>
      IO()
      Future {
        Thread.sleep(1000)
        1
      }
  }
}