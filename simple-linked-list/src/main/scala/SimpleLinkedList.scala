enum SimpleLinkedList[+A]:
  case Nil
  case Cons(head: A, tail: SimpleLinkedList[A])

object SimpleLinkedList:
  def apply[A](as: A*): SimpleLinkedList[A] =
    fromSeq(as)

  def fromSeq[A](xs: Seq[A]): SimpleLinkedList[A] =
    xs match
      case Seq()        => Nil
      case head +: tail => Cons(head, fromSeq(tail))

import SimpleLinkedList.*
extension [A](xs: SimpleLinkedList[A])
  def next: SimpleLinkedList[A] =
    xs match
      case Nil           => Nil
      case Cons(_, tail) => tail

  def add(item: A): SimpleLinkedList[A] =
    xs match
      case Nil              => Cons(item, Nil)
      case Cons(head, tail) => Cons(head, tail.add(item))

  def value: A =
    xs match
      case Nil           => throw new NoSuchElementException("empty list")
      case Cons(head, _) => head

  def isEmpty: Boolean =
    xs match
      case Nil           => true
      case Cons(head, _) => false

  def reverse: SimpleLinkedList[A] = xs.rev(Nil)

  private def rev(ys: SimpleLinkedList[A]): SimpleLinkedList[A] =
    xs match
      case Nil              => ys
      case Cons(head, tail) => tail.rev(Cons(head, ys))

  def toSeq: Seq[A] =
    xs match
      case Nil              => Seq.empty[A]
      case Cons(head, tail) => tail.toSeq.prepended(head)
