import scala.collection.mutable.Queue

class EmptyBufferException() extends Exception {}

class FullBufferException() extends Exception {}

class CircularBuffer(val capacity: Int):
  private val data = Queue.empty[Int]

  def write(value: Int) =
    if isFull then throw FullBufferException()
    data.enqueue(value)

  def read(): Int =
    if isEmpty then throw EmptyBufferException()
    data.dequeue()

  def overwrite(value: Int) =
    if isFull then data.dequeue()
    data.enqueue(value)

  def clear() =
    data.clear()

  private def isFull  = capacity == data.size
  private def isEmpty = data.isEmpty
