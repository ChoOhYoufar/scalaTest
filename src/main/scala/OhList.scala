/**
  * @author cho.oh 西暦18/07/11.
  */
trait MyListTrait[+A] {
  val head: A
  val tail: MyListTrait[A]
  def ::[B >: A](b: B) = MyList(b, this)
  //TODO 为什么用[B >: A](b: B)
//  def :::(b: MyList) = MyList(b, this)

//  def ::{}
  def :::[B >: A](b: MyList[B]): MyList[B] = b match {
      //MyList(1, MyNil):::MyList(2, MyNil)
      //MyList(1, 2, MyNil)
      //MyList(1, 2):::MyList(3, MyNil)
      //MyList(1, MyNil):::MyList(2, 3, MyNil)
    case b.tail == MyNil => b.head :: this
    case b.tail != MyNil => MyList(b.head, MyNil) ::: (b.tail :: this)
  }
  //
  def map[B](f: A => B): MyList[B] = B match {
    case MyNil => MyNil
    case MyList => f(this.head)
  }
//  def flatMap
//  def reverse
//
//  val days = List("a", "b")
//  days.head

//  def map = this match {
//  case MyList
//  case MyNil
//}

}

case class MyList[A] (head: A, tail: MyListTrait[A]) extends MyListTrait[A] {
//  override def ::: : Unit = ???
}

case object MyNil extends MyListTrait[Nothing] {
  override val head = throw new Exception
  override val tail = throw new Exception

//  override def ::: : Unit = ???
}

//MyList(1, MyList(2, MyList(3, MyNil)))
//1::MyList(2, MyNil)
val test = MyList(1, MyNil):::MyList(2, MyNil)
//MyList(1, MyNil).map(x => x + x)
MyList(1, 2, MyNil).map()
//print(test)

//sealed trait OhList[+A]
//case object Nil extends OhList[Nothing]
//case class Cons[+A](ohListHead: A, ohListTail: OhList[A]) extends OhList[A]
//
//object List {
//  def append[A](a1: List[A], a2: List[A]): a1 match {
//    case Nil => a2
//    case Cons(h, t) => Cons(h, append(t, a2))
//  }
//}