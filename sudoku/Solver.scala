class Solver(regionLen: Int) {
  val boardLen = regionLen * regionLen

  def solve(board: IndexedSeq[Int]): Option[IndexedSeq[Int]] = {
    if (board.size == boardLen * boardLen) solve(board, 0) else None
  }

  private def solve(board: IndexedSeq[Int], i: Int): Option[IndexedSeq[Int]] = {
    def candidates(i: Int) = {
      val (r, c) = (i / boardLen, i % boardLen)
      val row = board.slice(r * boardLen, (r + 1) * boardLen)
      val col = for (r <- 0 until boardLen) yield board(r * boardLen + c)
      val reg = {
        val corner = (r / regionLen * regionLen) * boardLen + (c / regionLen * regionLen)
        (for (p <- 0 until regionLen) yield board.slice(corner + p * boardLen, corner + p * boardLen + regionLen)).flatten
      }
      (1 to boardLen) diff (row ++ col ++ reg filter { _ > 0}).distinct
    }
    if (i == board.size) Some(board) else board(i) match {
      case 0 =>
        (Option.empty[IndexedSeq[Int]] /: candidates(i)) { case (r, n) =>
          r match {
            case None => solve(board.updated(i, n), i + 1)
            case _ => r
          }
        }
      case _ => solve(board, i + 1)
    }
  }
}
