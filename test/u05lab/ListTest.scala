package u05lab

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class ListTest {

  @Test
  def testZipRight(): Unit = {
    val l = List("a", "b", "c")

    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List(("a", 0), ("b", 1), ("c", 2)), l.zipRight)
  }

  @Test
  def testPartition(): Unit = {
    val l = 10 :: 20 :: 30 :: 40 :: Nil()
    assertEquals((Cons(20,Cons(30,Cons(40,Nil()))), Cons(10,Nil())), l.partition(_ > 15))
  }

  @Test
  def testSpan(): Unit = {
    val l = 10 :: 20 :: 30 :: 40 :: Nil()
    assertEquals((Nil(), Cons(10,Cons(20,Cons(30,Cons(40,Nil()))))), l.span(_ > 15))
  }

  @Test
  def testLength(): Unit = {
    val l = 10 :: 20 :: 30 :: 40 :: Nil()
    assertEquals(4, l.length())
    assertEquals(0, Nil().length())
  }

  @Test
  def testReduce(): Unit = {
    val l = 10 :: 20 :: 30 :: 40 :: Nil()
    assertEquals(100, l.reduce(_+_))
  }

  @Test
  def testTakeRight(): Unit = {
    val l = 10 :: 20 :: 30 :: 40 :: Nil()
    assertEquals(Cons(30, Cons(40, Nil())), l.takeRight(2))
  }

  @Test
  def testCollect(): Unit = {
    val l = 10 :: 20 :: 30 :: 40 :: Nil()
    assertEquals(Cons(9, Cons(39, Nil())), l.collect({ case x if x < 15 || x > 35 => x-1 }))
  }
}