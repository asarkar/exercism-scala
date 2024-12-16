import ForthError.*

class Forth extends ForthEvaluator:
  def eval(txt: String): Either[ForthError, ForthEvaluatorState] =
    val xs = txt.split(";").map(_.toUpperCase().strip()).filterNot(_.isEmpty())
    // Everything but the penultimate token are word definitions.
    // The last token is a command.
    val (defns, cmds) = xs.splitAt(xs.size - 1)

    Defn.parseDefns(defns) match
      case None => Left(InvalidWord)
      case Some(d) =>
        val defns = d.groupMapReduce(_._1)(v => IndexedSeq(v._2))(_ ++ _)
        val c = cmds.head.split("\\s+").flatMap { cmd =>
          if defns.contains(cmd) then Defn.resolveDefn(defns(cmd).last._1 + 1, cmd, defns)
          else Seq(cmd)
        }

        BuiltIn.run(c, ForthEvaluatorState(Seq.empty[Int]))
