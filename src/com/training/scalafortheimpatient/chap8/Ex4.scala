package com.training.scalafortheimpatient.chap8

object Ex4 {

  abstract class Item {
    def price : Double
    def description : String

    override def toString = s"Item($price, $description)"
  }

  case class SimpleItem(price : Double, description : String) extends Item

  case class Bundle(items : List[Item] = List[Item]()) extends Item {

    def add(item : Item) = new Bundle(items :+ item)
    def addAll(bundle : Bundle) = new Bundle(items ++: bundle.items)

    override def price: Double = {
      items.foldLeft(0.0)((sum : Double, item : Item) => item.price + sum)
    }

    override def description: String = {
      s"Bundle [$items]"
    }
  }
}
