import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


/** @version 3.2.0 */
class CryptoSquareTest extends AnyFunSuite with Matchers {

  test("empty plaintext results in an empty ciphertext") {
    CryptoSquare.ciphertext("") should be("")
  }

  test("Lowercase") {
    CryptoSquare.ciphertext("A") should be("a")
  }

  test("Remove spaces") {
    CryptoSquare.ciphertext("  b ") should be("b")
  }

  test("Remove punctuation") {
    CryptoSquare.ciphertext("@1,%!") should be("1")
  }

  test("9 character plaintext results in 3 chunks of 3 characters") {
    CryptoSquare.ciphertext("This is fun!") should be("tsf hiu isn")
  }

  test(
    "8 character plaintext results in 3 chunks, the last one with a trailing space") {
    CryptoSquare.ciphertext("Chill out.") should be("clu hlt io ")
  }

  test(
    "54 character plaintext results in 7 chunks, the last two with trailing spaces") {
    CryptoSquare.ciphertext(
      "If man was meant to stay on the ground, god would have given us roots.") should be(
      "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau ")
  }
}
