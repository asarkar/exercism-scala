import scala.util.matching.Regex
object PigLatin:
  private val rules =
    Seq(startsWithVowel, containsQu, containsVowel, containsY)

  def translate(s: String): String =
    s.split("\\s+")
      .collect(w => rules.map(_(w)).find(_.isDefined).flatten.getOrElse(w) + "ay")
      .mkString(" ")

  def startsWithVowel(s: String): Option[String] =
    if "^(?:[aeiou]+|xr|yt).*".r.matches(s) then Some(s) else None

  def containsQu(s: String): Option[String] =
    break(s, "qu".r).map((left, qu, right) => right + left + qu)

  def containsVowel(s: String): Option[String] =
    break(s, "[aeiou]".r).map((left, vowel, right) => vowel + right + left)

  def containsY(s: String): Option[String] =
    break(s, "(?<!y)y".r).map((left, y, right) => y + right + left)

  private def break(text: String, regex: Regex): Option[(String, String, String)] =
    regex.findFirstMatchIn(text) match
      case None    => None
      case Some(m) => Some(text.take(m.start), m.matched, text.drop(m.end))
