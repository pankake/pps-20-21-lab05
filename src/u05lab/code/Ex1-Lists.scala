
package u05lab.code

import scala.annotation.tailrec
import scala.language.postfixOps // silence warnings

sealed trait List[A] {

  def head: Option[A]

  def tail: Option[List[A]]

  def append(list: List[A]): List[A]

  def foreach(consumer: (A) => Unit): Unit

  def get(pos: Int): Option[A]

  def filter(predicate: (A) => Boolean): List[A]

  def map[B](fun: (A) => B): List[B]

  def toSeq: Seq[A]

  def foldLeft[B](acc: B)(f: (B,A)=>B): B

  def foldRight[B](acc: B)(f: (A,B)=>B): B

  def flatMap[B](f: A => List[B]): List[B]

  def reverse(): List[A]

  def zipRight: List[(A,Int)]

  def partition(pred: A => Boolean): (List[A],List[A])

  def span(pred: A => Boolean): (List[A],List[A])

  def reduce(op: (A,A)=>A): A

  def length(): Int

  def takeRight(n: Int): List[A]

  def collect[B](f: PartialFunction[A, B]): List[B]

  // right-associative construction: 10 :: 20 :: 30 :: Nil()
  def ::(head: A): List[A] = Cons(head,this)
}

// defining concrete implementations based on the same template

case class Cons[A](_head: A, _tail: List[A])
  extends ListImplementation[A] {
}

case class Nil[A]()
  extends ListImplementation[A]

// enabling pattern matching on ::

object :: {
  def unapply[A](l: List[A]): Option[(A,List[A])] = l match {
    case Cons(h,t) => Some((h,t))
    case _ => None
  }
}

// List algorithms
trait ListImplementation[A] extends List[A] {

  override def head: Option[A] = this match {
    case h :: _ => Some(h)
    case _ => None
  }
  override def tail: Option[List[A]] = this match {
    case _ :: t => Some(t)
    case _ => None
  }
  override def append(list: List[A]): List[A] = this match {
    case h :: t => h :: (t append list)
    case _ => list
  }
  override def foreach(consumer: (A)=>Unit): Unit = this match {
    case h :: t => {consumer(h); t foreach consumer}
    case _ => None
  }
  override def get(pos: Int): Option[A] = this match {
    case h :: _ if pos == 0 => Some(h)
    case _ :: t if pos > 0 => t get (pos-1)
    case _ => None
  }
  override def filter(predicate: (A) => Boolean): List[A] = this match {
    case h :: t if predicate(h) => h :: (t filter predicate)
    case _ :: t => (t filter predicate)
    case _ => Nil()
  }
  override def map[B](fun: (A) => B): List[B] = this match {
    case h :: t => fun(h) :: (t map fun)
    case _ => Nil()
  }

  override def toSeq: Seq[A] = this match {
    case h :: t => h +: t.toSeq // using method '+:' in Seq..
    case _ => Seq()
  }

  override def foldLeft[B](acc: B)(f: (B,A)=>B): B = this match {
    case Cons(h,t) => t.foldLeft(f(acc,h))(f)
    case Nil() => acc
  }

  override def foldRight[B](acc: B)(f: (A, B) => B): B =
    this.reverse().foldLeft(acc)((acc,elem) => f(elem,acc))

  override def reverse(): List[A] =
    this.foldLeft(Nil[A].asInstanceOf[List[A]])((acc,elem) => Cons(elem,acc))

  override def flatMap[B](f: A => List[B]): List[B] = this match {
    case Cons(h,t) => f(h).append(t.flatMap(f))
    case Nil() => Nil()
  }

  override def zipRight: List[(A,Int)] = {
    /*@tailrec
    def _zipRight(l: List[A], k: Int, res: List[(A, Int)]): List[(A, Int)] = l match {
      case h :: t => _zipRight(t, k + 1, (h, k) :: res)
      case _ => res
    }
    _zipRight(this, 0, List.nil).reverse()*/

    //var i = -1; map((_, { i += 1; i }))

    val i = Iterator.from(0)
    this.map((_ ,i.next))
  }

  /*override def partition(pred: A => Boolean): (List[A], List[A]) = {
    (filter(pred), filter(a => !pred(a)))
  }*/

  override def partition(pred: A => Boolean): (List[A], List[A]) = this.foldLeft((List[A](),List[A]())) ((acc, h)=> (acc, pred(h)) match {
    case ((a, b), true) => (a.append(h :: Nil()), b)
    case ((a, b), false) => (a, b.append(h:: Nil()))
  })

  override def span(pred: A => Boolean): (List[A],List[A]) = this.foldLeft((List[A](),List[A]())) ((acc, h)=> (acc, pred(h)) match {
    case ((a, hb :: tb), _) => (a, hb :: tb.append(h :: Nil()))
    case ((a, Nil()), true) => (a.append(h :: Nil()), Nil())
    case ((a, Nil()), false) => (a, h :: Nil())
  })

  /**
    *
    * @throws UnsupportedOperationException if the list is empty
    */

  override def length(): Int = {
    @tailrec
    def _length(l: List[A], n: Int): Int = l match {
      case _ :: Nil() => n
      case _ :: t => _length(t, n + 1)
      case Nil() => 0
    }
    _length(this, 1)
  }


  override def reduce(op: (A,A)=>A): A = this match {
    case h :: Nil() => h
    case h :: t => op(h, t.reduce(op))
    case Nil() => throw new UnsupportedOperationException()
  }

  override def takeRight(n: Int): List[A] = this.reverse() match {
    case h :: t if n > 0 => t.reverse().takeRight(n - 1) append (h :: Nil())
    case _ => Nil()
  }

  override def collect[B](f: PartialFunction[A, B]): List[B] = this match {
    case h :: t if f.isDefinedAt(h) => f(h) :: t.collect(f)
    case _ :: t => t.collect(f)
    case _ => Nil()
  }
}


// Factories
object List {

  // Smart constructors
  def nil[A]: List[A] = Nil()
  def cons[A](h: A, t: List[A]): List[A] = Cons(h,t)

  def apply[A](elems: A*): List[A] = {
    var list: List[A] = Nil()
    for (i <- elems.length-1 to 0 by -1) list = elems(i) :: list
    list
  }

  def of[A](elem: A, n: Int): List[A] =
    if (n==0) Nil() else elem :: of(elem,n-1)
}

object ListsTest extends App {

  import List._  // Working with the above lists
  println(List(10,20,30,40))
  val l = 10 :: 20 :: 30 :: 40 :: Nil() // same as above
  println(l.head) // 10
  println(l.tail) // 20,30,40
  println(l append l) // 10,20,30,40,10,20,30,40
  println(l append l toSeq) // as a list: 10,20,30,40,10,20,30,40
  println(l get 2) // 30
  println(of("a",10)) // a,a,a,..,a
  println(l filter (_<=20) map ("a"+_) ) // a10, a20

  assert(List(1,2,3) == List(1,2,3))

  println(scala.collection.immutable.List(10,20,30,40).partition(_>15))
  println(scala.collection.immutable.List(10,20,30,40).span(_>15))

  // Ex. 1: zipRight
  println(l.zipRight.toSeq) // List((10,0), (20,1), (30,2), (40,3))

  // Ex. 2: partition
  println(l.partition(_>15)) // ( Cons(20,Cons(30,Cons(40,Nil()))), Cons(10,Nil()) )

  // Ex. 3: span
  println(l.span(_>15)) // ( Nil(), Cons(10,Cons(20,Cons(30,Cons(40,Nil())))) )
  println(l.span(_<15)) // ( Cons(10,Nil()), Cons(20,Cons(30,Cons(40,Nil()))) )

  // Ex. 4: reduce
  println(l.reduce(_+_)) // 100
  try { List[Int]().reduce(_+_); assert(false) } catch { case _:UnsupportedOperationException => }

  // Ex. 5: takeRight
  println(l.takeRight(2)) // Cons(30,Cons(40,Nil()))

  // Ex. 6: collect
  // println(l.collect { case x if x<15 || x>35 => x-1 }) // Cons(9, Cons(39, Nil()))
}