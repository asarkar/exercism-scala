import scala.util.boundary, boundary.break
import collection.mutable.Stack

object MatchingBrackets:
  private val pairs = Map(']' -> '[', '}' -> '{', ')' -> '(')

  def isPaired(brackets: String): Boolean =
    val stack = Stack.empty[Char]
    // https://www.youtube.com/watch?v=0Fm0y4K4YO8
    // https://www.scala-lang.org/api/3.x/scala/util/boundary$.html
    boundary:
      for c <- brackets do
        if pairs.contains(c) && stack.headOption != pairs.get(c) then break(false)
        else if pairs.values.exists(_ == c) then stack.push(c)
        else if pairs.contains(c) then stack.pop()

      stack.isEmpty
