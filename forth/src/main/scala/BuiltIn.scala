import ForthError.*

object BuiltIn:
  private val NUM = """-?\d+""".r

  def run(cmds: Array[String], state: ForthEvaluatorState): Either[ForthError, ForthEvaluatorState] =
    if cmds.isEmpty then Right(state)
    else runBuiltInCmd(cmds.head, state).flatMap(run(cmds.tail, _))

  private def runBuiltInCmd(cmd: String, state: ForthEvaluatorState): Either[ForthError, ForthEvaluatorState] =
    if NUM.matches(cmd) then Right(state.copy(cmd.toInt +: state.s))
    else if cmd.size == 1 then runBinOp(cmd, state)
    else runStackOp(cmd, state)

  private def runBinOp(op: String, state: ForthEvaluatorState): Either[ForthError, ForthEvaluatorState] =
    state.s match
      case x +: y +: t =>
        op match
          case "+"           => Right(state.copy((x + y) +: t))
          case "-"           => Right(state.copy((y - x) +: t))
          case "*"           => Right(state.copy((x * y) +: t))
          case "/" if x != 0 => Right(state.copy((y / x) +: t))
          case "/"           => Left(DivisionByZero)
          case _             => Left(UnknownWord)
      case _ => Left(StackUnderflow)

  private def runStackOp(op: String, state: ForthEvaluatorState): Either[ForthError, ForthEvaluatorState] =
    (state.s, op) match
      // copy second stack item to top of stack
      case (_ +: y +: _, "OVER") => Right(state.copy(y +: state.s))
      // duplicate the top stack item
      case (x +: _, "DUP") => Right(state.copy(x +: state.s))
      // exchange the top 2 stack items
      case (x +: y +: xs, "SWAP") => Right(state.copy(y +: x +: xs))
      // remove top item from the stack
      case (_ +: xs, "DROP")                                     => Right(state.copy(xs))
      case _ if !Set("OVER", "DUP", "SWAP", "DROP").contains(op) => Left(UnknownWord)
      case _                                                     => Left(StackUnderflow)
