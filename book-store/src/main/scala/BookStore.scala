import collection.mutable.Map

object BookStore:
  private val COST_OF_ONE = 800
  private val DISCOUNTS   = IndexedSeq(0, 40, 80, 160, 200)

  private val memo = Map.empty[Seq[Int], Int]

  def total(books: Seq[Int]): Int =
    val freq = books
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .values
      .toSeq
      .sorted

    bagBooks(freq)

  /*
  For this problem, it only matters how many different types of books are there.
  At each iteration, we either put a book in the same basket, or in a new basket.
  Since we don't know any better, we have to try both options and see which basket
  ends up costing less.
  Instead of operating on the input array which may be large, we are going to be
  working with a frequency array which is limited to size 5.
  So, [1, 1, 2] becomes [2, 1].

  A naive caching/memoization approach would use the frequency array directly
  as the key, but note that [1, 2] and [2, 1, 0] cost exactly the
  same. Thus, we use a sorted, positives only, frequency array as the cache key.
  In the above example, it is [1, 2].
   */
  private def bagBooks(freq: Seq[Int]): Int =
    if memo.contains(freq) then return memo(freq)

    if freq.isEmpty then
      memo(freq) = 0
      return 0

    var cost = Int.MaxValue
    /*
    Different books that can be added to the basket are
    represented by the indices of the frequency array.
     */
    val remaining = (0 until freq.size).toSet.subsets().filterNot(_.isEmpty)

    for basket <- remaining do
      val n            = basket.size
      val costOfBasket = (COST_OF_ONE - DISCOUNTS(n - 1)) * n
      /*
      Decrement frequencies for the books in the basket
      and remove zero entries.
       */
      val newFreq = freq.zipWithIndex
        .filterNot((x, i) => basket.contains(i) && x == 1)
        .map { (x, i) =>
          x - (if basket.contains(i) then 1 else 0)
        }
        .sorted

      // Check the cost of the current basket, and start a new basket.
      cost = math.min(cost, costOfBasket + bagBooks(newFreq))

    memo(freq) = cost
    return cost
