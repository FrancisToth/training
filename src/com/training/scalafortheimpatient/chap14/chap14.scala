package com.training.scalafortheimpatient.chap14

object chap14 {
}

object Ex1 extends App {
}

object Ex2 extends App {
  def swap(pair:(Int,Int)) = pair match {
    case (x,y) => (y,x)
    case _ => pair
  }

  println(swap((1,2)))
}

object Ex3 extends App {
  def swap(arr:Seq[Int]) = arr match {
    case Seq(head,tail, _*) => (tail,head)
    case Seq(head,_*)  => (head,head)
    case _ => None
  }

  println(swap(Array(1,2)))
  println(swap(Array(1)))
  println(swap(Array(1,2,3)))
  println(swap(Seq[Int]()))
}

object Ex4 extends App {

  abstract class Item
  case class Article(description: String, price: Double) extends Item
  case class Bundle(description: String, discount: Double, items: Item*) extends Item

  case class Multiple(nb: Int, items:Item*) extends Item

  def price(it: Item): Double = it match {
    case Article(_, p) => p
    case Bundle(_, disc, its @ _*) => its.map(price).sum - disc
    case Multiple(nb, its) => nb * price(its)
  }

  val towel = Article("Towel", 20)
  val toaster = Article("toaster", 10)
  val multiple = Multiple(10, Bundle("Package", 5, towel, toaster))
  println(price(multiple)) // 250
  println(price(Multiple(2, multiple))) // 500
}

object Ex5 extends App {
  val list = List(List(3, 8), 2, List(5))
  def leafSum(list: List[Any]) : Int = list match {
    case (head : List[Any]) :: tail => leafSum(head) + leafSum(tail)
    case (head : Int) :: tail => head + leafSum(tail)
    case _ => 0
  }
  println(leafSum(list))
}

object Ex6 extends App {
  sealed abstract class BinaryTree
  case class Leaf(value: Int) extends BinaryTree
  case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

  val tree = Node(Node(Leaf(3), Leaf(8)), Leaf(2))

  def leafSum(tree: BinaryTree) : Int = tree match {
    case n: Node => leafSum(n.left) + leafSum(n.right)
    case l: Leaf => l.value
    case _ => 0
  }
  
  println(leafSum(tree))
}

object Ex7 extends App {
  sealed abstract class Tree
  case class Leaf(value: Int) extends Tree
  case class Node(nodes: Tree*) extends Tree

  val tree = Node(Node(Leaf(3), Leaf(8)), Leaf(2), Node(Leaf(5)))

  def leafSum(tree: Tree) : Int = tree match {
    case Node(n, r @ _*) => leafSum(n) + r.map(leafSum).sum
    case l: Leaf => l.value
    case _ => 0
  }

  println(leafSum(tree))
}

object Ex8 extends App {
  sealed abstract class Tree
  case class Leaf(value: Int) extends Tree
  case class Node(op:String, nodes: Tree*) extends Tree

  def leafSum(tree: Tree) : Int = tree match {
    case l: Leaf => l.value
    case Node(op, n) => eval(op, 0, leafSum(n))
    case Node(op, n, r @ _*) => r.foldLeft(leafSum(n))((t, tree) => eval(op, t, leafSum(tree)))
    case _ => 0
  }

  def eval(op:String, left:Int, right:Int) = op match {
    case "+" => left + right
    case "-" => left - right
    case "*" => left * right
  }

  val tree = Node("+", Node("*",Leaf(3), Leaf(8)), Leaf(2), Node("-", Leaf(5)))
  println(leafSum(tree))
}

object Ex9 extends App {
  val list : List[Option[Int]] = List(Option(1), Option(2), Option(3))
  def sum(list: List[Option[Int]]) : Int = {
    list.collect { case Some(x) => x }.sum
  }
  println(sum(list))
}

object Ex10 extends App {
  def compose(f:(Double)=> Option[Double], g:(Double)=> Option[Double]) : (Double)=> Option[Double] = {
    (x:Double) => {
      g(x) match {
        case Some(Double.PositiveInfinity | Double.NegativeInfinity) => None
        case Some(result) => f(result)
        case _ => None

      }
    }
  }

  def f(x:Double) = if(x >= 0) Some(scala.math.sqrt(x)) else None
  def g(x:Double) = if(x != 0) Some(1 / (x-1)) else None

  val h = compose(f,g)

  println(h(2))
  println(h(1))
  println(h(0))
}

object Ex11 extends App {

//  case class Leaf(key:Option[String]=None, value:Option[String]=None)
//  case class Node(key:Option[String]=None, value:Option[String]=None)
//  case class Tree(parent:Option[Leaf]=None, value:Option[String]=None) extends Leaf


  /*case class Node(parent:Option[Node]=None, children:List[Node]=List(),
                  key:Option[String]=None, value:Option[String]=None ) {

    def addChild(node:Node) = this.copy(children = children :+ node)
    def isRoot = parent.isDefined

    def allNodes : Seq[Node] = crawlDown(identity)

    private def crawlDown[T](f:(Node) => T) : Seq[T] = {
      Seq(f(this)) ++ (children match {
        case Nil => Seq()
        case head :: Nil => head.crawlDown(f)
        case head :: tail => head.crawlDown(f) ++ tail.flatMap(_.crawlDown(f))
      })
    }

    def path : Seq[String] = crawlUp(_.key.getOrElse(""))

    private def crawlUp[T](f:(Node) => T) : Seq[T] = {
      Seq(f(this)) ++ (parent match {
        case Some(n) => n.crawlUp(f)
        case _ => Seq()
      })
    }

    override def toString: String = {
      val valueAsStr = value.getOrElse("")
      children match {
        case Nil => s"[$valueAsStr]"
        case head::tail => s"$valueAsStr"
      }
    }
  }

  val i = Node(key=Some("7"),value=Some("I"))
  val j = Node(key=Some("8"),value=Some("J"))
  val e = Node(key=Some("4"),value=Some("E"), children=List(i,j))
  val f = Node(key=Some("5"),value=Some("F"))
  val g = Node(key=Some("6"),value=Some("G"))
  val b = Node(key=Some("1"),value=Some("B"), children=List(e,f))
  val c = Node(key=Some("2"),value=Some("C"), children=List(g))
  val d = Node(key=Some("3"),value=Some("D"))
  val a = Node(key=Some("0"),value=Some("A"), children=List(b,c,d))*/

//  println(a)
//  println(i.path)
//  println(Node(value=Some("A")) >>: Node(value=Some("B")) >>: Node(value=Some("C")))

  // Node("A") > Node("B") :: Node("C") :: Node("D") > Node("E") :: Node("F") :: Node("G") > Node("I") :: Node("J")

//  val nodes = Node(value=Some("A"), children=
//    List(Node(value=Some("B"), children=
//      List())))
//
//  println(nodes)

//
//    public Set<String> getAllKeys()
//    {
//      def allKeys = new HashSet()
//      def closure = {Node node -> allKeys.add(node.key)}
//      crawlDown(closure)
//
//      return allKeys
//    }
//
//    public List<Node> getAllNodes()
//    {
//      def allNodes = []
//      def closure = {Node node -> allNodes.add(node)}
//      crawlDown(closure)
//
//      return allNodes
//    }
//
//    public List<String> getPath()
//    {
//      def path = []
//      def closure = {Node node -> if (node.key) path.add(node.key)}
//      crawlUp(closure)
//
//      return path
//    }
//
//    private crawlDown(Closure closure)
//  {
//    closure(this)
//    children.each {it.crawlDown(closure)}
//  }
//
//    private crawlUp(Closure closure)
//  {
//    closure(this)
//    if (parent)
//    {
//      parent.crawlUp(closure)
//    }
//  }
//
//  }

}