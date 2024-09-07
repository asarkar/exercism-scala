import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


/** @version created manually **/
class DequeTest extends AnyFlatSpec with Matchers {

  it should "handle push then pop" in {
    val deque = Deque[Char]()
    deque.push('a')
    deque.push('b')
    deque.pop should be (Some('b'))
    deque.pop should be (Some('a'))
//    linkedList.pop should be (None)
  }

  it should "handle push then shift" in {
    val deque = Deque[Char]()
    deque.push('a')
    deque.push('b')
    deque.shift should be (Some('a'))
    deque.shift should be (Some('b'))
//    linkedList.shift should be (None)
  }

  it should "handle unshift then shift" in {
    val deque = Deque[Char]()
    deque.unshift('a')
    deque.unshift('b')
    deque.shift should be (Some('b'))
    deque.shift should be (Some('a'))
  }

  it should "handle unshift then pop" in {
    val deque = Deque[Char]()
    deque.unshift('a')
    deque.unshift('b')
    deque.pop should be (Some('a'))
    deque.pop should be (Some('b'))
  }

  it should "handle complex interaction" in {
    val deque = Deque[Int]()
    deque.push(1)
    deque.push(2)
    deque.pop should be (Some(2))
    deque.push(3)
    deque.unshift(4)
    deque.push(5)
    deque.shift should be (Some(4))
    deque.pop should be (Some(5))
    deque.pop should be (Some(3))
  }

  it should "handle push, unshift, pop, shift, shift, in that order" in {
    val deque = Deque[Int]()
    deque.push(1)
    deque.unshift(5)
    deque.pop should be (Some(1))
    deque.shift should be (Some(5))
    deque.shift should be (None)
  }

  it should "handle push, push, shift, shift, push, shift, in that order" in {
    val deque = Deque[Int]()
    deque.push(41)
    deque.push(59)
    deque.shift should be (Some(41))
    deque.shift should be (Some(59))
    deque.push(47)
    deque.shift should be (Some(47))
  }
}
