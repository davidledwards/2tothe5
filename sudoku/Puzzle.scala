object Puzzle {
  def main(args: Array[String]): Unit = {
    val board = IndexedSeq(
      0, 0, 0, 0, 0, 7, 0, 4, 0,
      0, 2, 0, 4, 5, 0, 7, 0, 0,
      0, 0, 5, 0, 2, 0, 0, 0, 8,
      0, 0, 0, 0, 0, 0, 0, 1, 7,
      5, 0, 0, 6, 0, 9, 0, 0, 3,
      8, 7, 0, 0, 0, 0, 0, 0, 0,
      4, 0, 0, 0, 6, 0, 9, 0, 0,
      0, 0, 2, 0, 4, 3, 0, 6, 0,
      0, 6, 0, 5, 0, 0, 0, 0, 0
    )

    val solver = new Solver(3)
    solver.solve(board) match {
      case Some(b) =>
        for (r <- 0 until 9) {
          for (c <- 0 until 9) print(b(r * 9 + c) + " ")
          println()
        }
      case None =>
        println("unsolvable")
    }
  }
}
