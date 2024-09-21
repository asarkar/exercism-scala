enum Sublist:
  case Sublist, Superlist, Equal, Unequal

private enum Tree[+A]:
  case Nil
  case Node(top: Seq[A], next: () => Tree[A], rest: Tree[A])

object Tree:
  def node[A](top: Seq[A], next: => Tree[A], rest: Tree[A]): Tree[A] =
    lazy val n = next
    Node(top, () => n, rest)

  def empty[A]: Tree[A] = Nil

import Sublist.*
object Sublist:
  def sublist(l1: Seq[Int], l2: Seq[Int]): Sublist =
    if isSublist(l1, l2) then
      if l1 == l2 then Equal
      else Sublist
    else if isSublist(l2, l1) then Superlist
    else Unequal

  import Tree.*

  /*
  This is an implementation of the KMP algorithm in a functional manner that doesn't use
  any mutable data structures. It is based on a Haskell implementation from the following
  paper:
  https://www.cambridge.org/core/journals/journal-of-functional-programming/article/knuthmorrispratt-illustrated/8EFA77D663D585B68630E372BCE1EBA4

  In order for this algorithm to work, we need to evaluate 'next' parameter of the Node class
  lazily, because there's a circularity that arises becauase init is defined in terms of make,
  which calls step, which returns init in the base case. Haskell is lazy by default, but in
  Scala, we follow the pattern shown in section 5.2 of the book "Functional Programming in Scala",
  second edition. The following StackOverflow thread shows how to implement lazy evalation using call-by-name.
  https://stackoverflow.com/a/26750007/839733

  Lastly, the following document shows few dry runs of this implementation:
  https://github.com/asarkar/exercism-haskell/blob/master/sublist/kmp.docx
   */
  private def isSublist[A](l1: Seq[A], l2: Seq[A]): Boolean =
    def make(xs: Seq[A], t: Tree[A]): Tree[A] =
      xs match
        case Seq() => node(xs, Nil, t)
        case head +: tail =>
          val rest = t match
            case n: Node[A] if check(n, head) => n.rest
            case _                            => t

          node(xs, make(tail, step(t, head)), rest)

    def init: Tree[A] = make(l1, Nil)

    def step(t: Tree[A], x: A): Tree[A] =
      t match
        case Nil                       => init
        case n @ Node(top, next, rest) => if check(n, x) then next() else step(rest, x)

    def check(t: Tree[A], x: A): Boolean =
      t match
        case Node((y :: _), _, _) => x == y
        case _                    => false

    def done(t: Tree[A]): Boolean =
      t match
        case Node(top, _, _) => top.isEmpty
        case _               => false

    l2
      .scanLeft(init)(step)
      .exists(done)
