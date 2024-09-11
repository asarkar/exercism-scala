object BookStore2:
  private val COST_OF_ONE = 800
  private val DISCOUNTS   = IndexedSeq(0, 40, 80, 160, 200)

  // For a combinatorial solution, see
  // https://exercism.org/tracks/scala/exercises/book-store/solutions/vgrigoriu
  def total(books: Seq[Int]): Int =
    if books.isEmpty then return 0

    // Calculate the frequency of books and sort descending.
    val freq = books
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .values
      .toSeq
      .sorted(summon[Ordering[Int]].reverse)

    Seq(greedy, balanced)
      .map(f => cost(f(freq)))
      .min

  private def cost(baskets: Iterable[Int]): Int =
    baskets
      .map(n => (COST_OF_ONE - DISCOUNTS(n - 1)) * n)
      .sum

  /*
  Puts as many _different_ books in a basket as
  possible before moving on to the next basket.
  Example:
    freq = [2, 2, 1, 1]
    distribution of books into baskets = [0, 0, 0, 0, 1, 1]
    count of different books in the baskets = [4, 2]
   */
  private def greedy(freq: Seq[Int]): Iterable[Int] =
    Iterable.unfold(1) { basket =>
      val n = freq
        .takeWhile(_ >= basket)
        .length
      Option.when(n > 0)((n, basket + 1))
    }

  /*
  Picks up a pile of the _same_ book and puts one
  in each basket. Then picks up the next pile, and
  starts with the basket next to the one last used,
  wrapping around if necessary.
  The number of baskets is equal to the number
  of different books.
  Example:
    freq = [2, 2, 1, 1]
    # of baskets = 2, numbered 0 and 1
    distribution of books into baskets = [0, 1, 0, 1, 0, 1]
    count of different books in the baskets = [3, 3]
   */
  private def balanced(freq: Seq[Int]): Iterable[Int] =
    val cumFreq = (0, freq.head) +: freq
      .zip(freq.tail)
      .map((prev, curr) => (prev, prev + curr))

    val numBaskets = freq.head

    cumFreq
      .flatMap((i, j) => (i until j).map(_ % numBaskets))
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .values
