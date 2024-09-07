import scala.util.Random

class Cipher(val key: String):
  def encode(plain: String): String  = xcode(plain, _ + _)
  def decode(cipher: String): String = xcode(cipher, _ - _)

  private def cycle(s: String): Iterator[Char] = Iterator.continually(s).flatten

  private def xcode(text: String, f: (Int, Int) => Int): String =
    cycle(key)
      .zip(text)
      .map { (x, y) =>
        val offset = math.floorMod(f(y - 'a', x - 'a'), 26)
        (offset + 'a').toChar
      }
      .mkString

object Cipher:
  def apply(key: Option[String]): Cipher =
    val k = key match
      case None =>
        Random.alphanumeric
          .filter(_.isLower)
          .take(10)
          .mkString
      case Some(value) if !value.isEmpty() && value.forall(_.isLower) => value
      case _                                                          => throw new IllegalArgumentException()

    new Cipher(k)
