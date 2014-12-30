package aima.search

import junit.framework._
import Assert._

class FifoQueueTest extends TestCase {
  def testIt() {
    val f = new FifoQueue[Int]
    assertTrue(f.isEmpty)
    f.insert(1)
    f.insertAll(List(2,3))
    f.insertAll(List(4,5))
    assertEquals(f.removeFirst,Some(1))
    assertEquals(f.removeFirst,Some(2))
    assertEquals(f.removeFirst,Some(3))
    assertEquals(f.removeFirst,Some(4))
    assertEquals(f.removeFirst,Some(5))
    assertTrue(f.isEmpty)
  }
}

class LifoQueueTest extends TestCase {
  def testIt() {
    val f = new LifoQueue[Int]
    assert(f.isEmpty)
    f.insert(1)
    f.insertAll(List(2,3))
    f.insertAll(List(4,5))
    assertEquals(f.removeFirst,Some(5))
    assertEquals(f.removeFirst,Some(4))
    assertEquals(f.removeFirst,Some(3))
    assertEquals(f.removeFirst,Some(2))
    assertEquals(f.removeFirst,Some(1))
    assert(f.isEmpty)
  }
}

class PriorityQueueTest extends TestCase {
  def testIt() {
    val f = new PriorityQueue[Int](
      new Ordering[Int] {
        def compare(a: Int, b: Int) = b-a })
                    
    assert(f.isEmpty)
    f.insert(1)
    f.insertAll(List(2,3))
    f.insertAll(List(4,5))
    assertEquals(f.removeFirst,Some(1))
    assertEquals(f.removeFirst,Some(2))
    assertEquals(f.removeFirst,Some(3))
    assertEquals(f.removeFirst,Some(4))
    assertEquals(f.removeFirst,Some(5))
    assert(f.isEmpty)
  }
}
