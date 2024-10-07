private enum CustomSet:
  case Nil
  case Node(left: CustomSet, x: Int, right: CustomSet)

import CustomSet.*
extension (cs: CustomSet)
  private def foldl[A](z: A)(op: (A, Int) => A): A =
    cs match
      case Nil                  => z
      case Node(left, x, right) => right.foldl(op(left.foldl(z)(op), x))(op)

  private def toList: Seq[Int] = cs.foldl(Seq.empty)((xs, x) => x +: xs)

object CustomSet:
  def fromList(xs: Seq[Int]) =
    xs.foldLeft(Nil)(insert)

  def empty(xs: CustomSet): Boolean =
    xs == Nil

  def member(xs: CustomSet, x: Int): Boolean =
    xs match
      case Nil => false
      case Node(left, y, right) =>
        if x < y then member(left, x)
        else if x > y then member(right, x)
        else true

  def isSubsetOf(xs: CustomSet, ys: CustomSet): Boolean =
    // Could be shortended to xs.foldl(true)((b, x) => b && member(ys, x)),
    // but it checks all elements even if a false is found.
    xs match
      case Nil                  => true
      case Node(left, x, right) => member(ys, x) && isSubsetOf(left, ys) && isSubsetOf(right, ys)

  def isDisjointFrom(xs: CustomSet, ys: CustomSet): Boolean =
    empty(intersection(xs, ys))

  def isEqual(xs: CustomSet, ys: CustomSet): Boolean =
    xs.toList == ys.toList

  def insert(xs: CustomSet, x: Int): CustomSet =
    xs match
      case Nil => Node(Nil, x, Nil)
      case n @ Node(left, y, right) =>
        if x < y then Node(insert(left, x), y, right)
        else if x > y then Node(left, y, insert(right, x))
        else n

  def intersection(xs: CustomSet, ys: CustomSet): CustomSet =
    xs.foldl(Nil)((zs, x) => if member(ys, x) then insert(zs, x) else zs)

  def difference(xs: CustomSet, ys: CustomSet): CustomSet =
    xs.foldl(Nil)((zs, x) => if !member(ys, x) then insert(zs, x) else zs)

  def union(xs: CustomSet, ys: CustomSet): CustomSet =
    xs.foldl(ys)(insert)
