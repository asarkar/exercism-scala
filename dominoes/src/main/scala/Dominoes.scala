import scala.util.boundary, boundary.break

import scala.collection.mutable.Stack

object Dominoes:
  def chain(input: Seq[(Int, Int)]): Option[List[(Int, Int)]] =
    val dominoes = input.toArray
    val fvm      = FaceValueMap(dominoes)
    val result   = Stack.empty[(Int, Int)]

    def solve(left: Boolean, i: Int): Boolean =
      val (l, r) = dominoes(i)
      result.push(if left then (l, r) else (r, l))
      fvm.acquire(i)

      boundary:
        for j <- fvm.candidates(i, left) do
          val x = dominoes(j)._2
          /*
          If dominoes[j] is matched on the right side,
          then its candidates should be matched on
          their left.
           */
          if solve(Seq(l, r).contains(x), j) then break(true)

      fvm.release(i)

      if result.size == dominoes.size then result.head._1 == result.last._2
      else
        result.pop()
        false

    if dominoes.isEmpty then Some(List.empty)
    else if solve(false, 0) then Some(result.toList)
    else None
