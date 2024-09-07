enum Bst[+A]:
  case Empty
  case Branch(value: A, left: Bst[A], right: Bst[A])

object Bst:
  def apply[A: Ordering](a: A): Bst[A] = fromList(Seq(a))

  def fromList[A: Ordering](as: Seq[A]): Bst[A] =
    as.foldLeft(Empty)((tree, a) => tree.insert(a))

  def toList[A](bst: Bst[A]): List[A] =
    bst match
      case Empty                      => Nil
      case Branch(value, left, right) => Bst.toList(left) ++ (value :: Bst.toList(right))

import Bst.*
extension [A: Ordering](bst: Bst[A])
  def value: A = bst match
    case Bst.Branch(a, left, right) => a
    case _                          => throw new NoSuchElementException("empty tree")

  def insert(a: A): Bst[A] =
    bst match
      case Bst.Branch(x, left, right) if summon[Ordering[A]].lteq(a, x) => Branch(x, left.insert(a), right)
      case Bst.Branch(x, left, right)                                   => Branch(x, left, right.insert(a))
      case Empty                                                        => Branch(a, Empty, Empty)

  def left: Option[Bst[A]] =
    bst match
      case Empty                      => None
      case Branch(value, left, right) => Some(left)

  def right: Option[Bst[A]] =
    bst match
      case Empty                      => None
      case Branch(value, left, right) => Some(right)
