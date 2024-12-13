import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import Sgf._

/** @version created manually **/
class SgfTest extends AnyFunSuite with Matchers {
  test("parse \"\"") {
    parseSgf("") should be (None)
  }

  test("parse \"()\"") {
    parseSgf("()") should be (None)
  }

  test("parse \";\"") {
    parseSgf(";") should be (None)
  }

  test("parse \"(;)\"") {
    parseSgf("(;)") should be (Some(Node(Map())))
  }

  test("parse \"(;A[B])\"") {
    parseSgf("(;A[B])") should be (Some(Node(Map("A" -> List("B")))))
  }

  test("parse \"(;a)\"") {
    parseSgf("(;a)") should be (None)
  }

  test("parse \"(;a[b])\"") {
    parseSgf("(;a[b])") should be (None)
  }

  test("parse \"(;Aa[b])\"") {
    parseSgf("(;Aa[b])") should be (None)
  }

  test("parse \"(;A[B];B[C])\"") {
    parseSgf("(;A[B];B[C])") should be (
        Some(Node(Map("A" -> List("B")), List(Node(Map("B" -> List("C")))))))
  }

  test("parse \"(;A[B](;B[C])(;C[D]))\"") {
    parseSgf("(;A[B](;B[C])(;C[D]))") should be (
        Some(Node(Map("A" -> List("B")), List(Node(Map("B" -> List("C"))),
                                              Node(Map("C" -> List("D")))))))
  }

  test("parse \"(;A[b][c][d])\"") {
    parseSgf("(;A[b][c][d])") should be (Some(Node(Map("A" -> List("b", "c", "d")))))
  }

  test("""parse "(;A[\\]b\nc\\\nd\t\te\\\\ \\\n\\]])"""") {
    parseSgf("(;A[\\]b\nc\\\nd\t\te\\\\ \\\n\\]])") should be (
        Some(Node(Map("A" -> List("]b cd  e\\ ]")))))
  }
}
