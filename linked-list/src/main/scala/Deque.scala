private case class Node[T](val value: T, var next: Option[Node[T]], var prev: Option[Node[T]])

class Deque[T]:
  var first: Option[Node[T]] = None
  var last: Option[Node[T]]  = None

  def singleton(value: T) =
    val x = new Node(value, None, None)
    first = Some(x)
    last = first

  // Appends the given value at the end
  def push(value: T): Unit = last match
    case Some(x) =>
      last = Some(new Node(value, x.next, last))
      x.next = last
    case _ => singleton(value)

  // Prepends the given value at the beginning
  def unshift(value: T): Unit = first match
    case Some(x) =>
      first = Some(new Node(value, first, x.prev))
      x.prev = first
    case _ => singleton(value)

  // Removes the last element of the list
  def pop: Option[T] = last match
    case Some(x) =>
      last = x.prev
      last match
        case None        => first = None
        case Some(value) => value.next = None

      Some(x.value)
    case _ => None

  // Removes the first element of the list
  def shift: Option[T] = first match
    case Some(x) =>
      first = x.next
      first match
        case None        => last = None
        case Some(value) => value.prev = None

      Some(x.value)
    case _ => None
