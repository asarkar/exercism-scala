case class WordCount(line: String):
  private val word = "([a-zA-Z0-9]+)('[a-zA-Z0-9]+)?".r.unanchored

  val countWords =
    val words = for (m <- word.findAllMatchIn(line)) yield m.matched.toLowerCase()
    words.toSeq.groupMapReduce(identity)(_ => 1)(_ + _)
