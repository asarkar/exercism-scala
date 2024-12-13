enum Color(val player: Char):
  case Black extends Color('X')
  case White extends Color('O')

class Connect private (board: IndexedSeq[String]):
  private val m                  = board.size
  private val n                  = board.head.size
  private val topRow: Seq[Cell]  = (0 until n).map((0, _))
  private val leftCol: Seq[Cell] = (0 until m).map((_, 0))

  opaque type Cell = (Int, Int)

  def winner: Option[Color] =
    val winner = Seq(('O', topRow), ('X', leftCol))
      .find((player, cells) => cells.exists(cell => getVal(cell) == player && bfs(Seq(cell), player)))

    winner.flatMap((w, _) => Color.values.find(_.player == w))

  private def hasWon(cell: Cell, player: Char): Boolean =
    (player == 'O' && cell._1 == m - 1) || (player == 'X' && cell._2 == n - 1)

  private def getVal(cell: Cell): Char =
    board.lift(cell._1).flatMap(_.lift(cell._2)).getOrElse('\u0000')

  private def neighbors(cell: Cell, visited: Set[Cell]): Seq[Cell] =
    for {
      nxt <- Seq(
        (cell._1 - 1, cell._2),
        (cell._1 - 1, cell._2 + 1),
        (cell._1, cell._2 - 1),
        (cell._1, cell._2 + 1),
        (cell._1 + 1, cell._2 - 1),
        (cell._1 + 1, cell._2)
      ) if getVal(nxt) == getVal(cell) && !visited.contains(nxt)
    } yield nxt

  private def bfs(q: Seq[Cell], player: Char, visited: Set[Cell] = Set.empty): Boolean =
    q match
      case Seq()                       => false
      case h +: t if hasWon(h, player) => true
      case h +: t                      => bfs(t ++ neighbors(h, visited), player, visited + h)

object Connect:
  def apply(board: Seq[String]): Connect = new Connect(board.toIndexedSeq)
