enum ForthError:
  case DivisionByZero, StackUnderflow, InvalidWord, UnknownWord

case class ForthEvaluatorState(s: Seq[Int]):
  override def toString(): String = s.reverse.mkString(" ")

trait ForthEvaluator:
  def eval(txt: String): Either[ForthError, ForthEvaluatorState]
