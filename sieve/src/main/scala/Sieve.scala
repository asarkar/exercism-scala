import java.util.PriorityQueue
import scala.collection.mutable.Buffer

object Sieve:
  def primes(limit: Int): Seq[Int] =
    val composites = new PriorityQueue[(Int, Int)](summon[Ordering[(Int, Int)]])
    val ans        = Buffer.empty[Int]
    val n          = math.sqrt(limit + 1).intValue

    for i <- 2 to limit do
      if composites.isEmpty || composites.peek()._1 != i then
        ans += i
        if i <= n then composites.offer(i * i, i)
      else
        while !composites.isEmpty() && composites.peek()._1 == i do
          val (x, y) = composites.poll()
          composites.offer(x + y, y)

    ans.toSeq
