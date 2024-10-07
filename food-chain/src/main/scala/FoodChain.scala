object FoodChain:
  private case class Animal(name: String, phrase: String = "", extra: String = "", ladykiller: Boolean = false)

  private val chain = IndexedSeq(
    Animal("fly"),
    Animal(
      "spider",
      "It wriggled and jiggled and tickled inside her.",
      extra = " that wriggled and jiggled and tickled inside her"
    ),
    Animal("bird", "How absurd to swallow a bird!"),
    Animal("cat", "Imagine that, to swallow a cat!"),
    Animal("dog", "What a hog, to swallow a dog!"),
    Animal("goat", "Just opened her throat and swallowed a goat!"),
    Animal("cow", "I don't know how she swallowed a cow!"),
    Animal("horse", "She's dead, of course!", ladykiller = true)
  )
  def recite(startVerse: Int, endVerse: Int): String =
    val verses = for n <- startVerse to endVerse yield verse(n - 1)
    verses.map(_.mkString("\n")).mkString("\n\n") + "\n\n"

  private def verse(n: Int): Seq[String] =
    val animal = chain(n)
    val start  = iKnow(animal)
    if !animal.ladykiller then
      val lines = for i <- n to 1 by -1 yield hunt(chain(i), chain(i - 1))
      start ++ lines :+ iDontKnow
    else start

  private def iKnow(animal: Animal): Seq[String] =
    val lines = Seq(s"I know an old lady who swallowed a ${animal.name}.")
    if !animal.phrase.isEmpty then lines :+ animal.phrase
    else lines

  private def hunt(predator: Animal, prey: Animal): String =
    s"She swallowed the ${predator.name} to catch the ${prey.name}${prey.extra}."

  private def iDontKnow: String =
    "I don't know why she swallowed the fly. Perhaps she'll die."
