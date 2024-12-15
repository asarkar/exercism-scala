case class BinTree[A](value: A, left: Option[BinTree[A]], right: Option[BinTree[A]])

sealed trait Crumb[A]
case class LeftCrumb[A](v: A, t: Option[BinTree[A]])  extends Crumb[A]
case class RightCrumb[A](v: A, t: Option[BinTree[A]]) extends Crumb[A]

opaque type Breadcrumbs[A] = Seq[Crumb[A]]

opaque type Zipper[A] = (BinTree[A], Breadcrumbs[A])

object Zipper:
  // Get a zipper focussed on the root node.
  def fromTree[A](t: BinTree[A]): Zipper[A] = (t, Seq.empty)

  // Get the complete tree from a zipper.
  def toTree[A](z: Zipper[A]): BinTree[A] = up(z) match
    case Some(parent) => toTree(parent)
    case _            => z._1

  // Get the value of the focus node.
  def value[A](z: Zipper[A]): A =
    z._1.value

  // Get the left child of the focus node, if any.
  def left[A](z: Zipper[A]): Option[Zipper[A]] = z match
    case (BinTree(x, Some(l), r), xs) => Some((l, LeftCrumb(x, r) +: xs))
    case _                            => None

  // Get the right child of the focus node, if any.
  def right[A](z: Zipper[A]): Option[Zipper[A]] = z match
    case (BinTree(x, l, Some(r)), xs) => Some((r, RightCrumb(x, l) +: xs))
    case _                            => None

  // Get the parent of the focus node, if any.
  def up[A](z: Zipper[A]): Option[Zipper[A]] = z match
    case (t, LeftCrumb(x, r) +: xs)  => Some((BinTree(x, Some(t), r), xs))
    case (t, RightCrumb(x, l) +: xs) => Some((BinTree(x, l, Some(t)), xs))
    case _                           => None

  // Set the value of the focus node.
  def setValue[A](v: A, z: Zipper[A]): Zipper[A] = z match
    case (BinTree(_, l, r), xs) => (BinTree(v, l, r), xs)

  // Replace a left child tree.
  def setLeft[A](l: Option[BinTree[A]], z: Zipper[A]): Zipper[A] = z match
    case (BinTree(x, _, r), xs) => (BinTree(x, l, r), xs)

  // Replace a right child tree.
  def setRight[A](r: Option[BinTree[A]], z: Zipper[A]): Zipper[A] = z match
    case (BinTree(x, l, _), xs) => (BinTree(x, l, r), xs)
