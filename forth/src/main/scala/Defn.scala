object Defn:
  private val _ASCII_ALPHA = """[A-Za-z]+""".r
  private val _BIN_OP      = """[+\-*\\]""".r
  private val WORD = s"""
                        |^
                        |:
                        |\\s+
                        |(
                        |  $_ASCII_ALPHA(?:-$_ASCII_ALPHA)*
                        |  |
                        |  $_BIN_OP
                        |)
                        |\\s+
    """.stripMargin.replaceAll("\\s+", "").r
  private val DEFN = """([^;\s]+)""".r

  // Parses ": word definition ;"
  private def parseDefn(txt: String): Option[(String, Seq[String])] =
    WORD.findFirstMatchIn(txt) match
      case Some(m) =>
        val word = m.group(1)
        val defn = DEFN.findAllIn(txt.substring(m.end(1))).toSeq
        Some((word, defn))
      case _ => None

  /*
  Stores the definition as raw text, doesn't evaluate eagerly.
  Since a word may later be redefined, the definitions are stored with monotonically increasing ids,
  thus establishing a happens-before relationship between any two definitions.
  The latest definition is at the end.
   */
  def parseDefns(definitions: Array[String], i: Int = 0): Option[Seq[(String, (Int, Seq[String]))]] =
    if definitions.isEmpty then Some(Seq.empty)
    else
      parseDefn(definitions.head) match
        case Some((w, defns)) => parseDefns(definitions.tail, i + 1).map((w, (i, defns)) +: _)
        case _                => None

  /*
  Resolves a definition by recursively replacing all user-defined words with built-in commands.
  If a word is not found in the dictionary, it could be a built-in word, or an invalid one,
  to be find out later whe parsing it as a built-in word.

  Definitions are processed in the reverse order because later definitions may refer to earlier ones,
  like ": foo foo 1 + ;". The second "foo" must be resolved first.

  For each word in the definition, finds the definition with the greatest id that is smaller than the
  given definition's id. That gives the latest definition at the time this definition of word existed.
   */
  def resolveDefn(i: Int, word: String, definitions: Map[String, IndexedSeq[(Int, Seq[String])]]): Seq[String] =
    if !definitions.contains(word) then Seq(word)
    else
      val j          = definitions(word).search((i, Seq.empty))(Ordering.by(_._1)).insertionPoint
      val (k, defns) = definitions(word)(j - 1)

      defns.foldRight(Seq.empty)((w, acc) => resolveDefn(k, w, definitions) ++ acc)
