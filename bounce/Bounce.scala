import scala.util.Random.{nextInt => rand}
object Bounce {
  def main(args: Array[String]): Unit = {
    val (xsize, ysize) = (args(0).toInt, args(1).toInt)
    print(s"\u001b[2J\u001b[0;0H")
    val lines = (for (_ <- 1 to 50) yield {
      val (x, y) = (rand(xsize / 2), rand(ysize / 2))
      (if (rand(2) == 0) for (_x <- x until x + rand(xsize - x)) yield (_x, y) -> 0 else for (_y <- y until y + rand(ysize - y)) yield (x, _y) -> 1).toMap
    }).flatten.toMap
    for (((x, y), line) <- lines) print(s"\u001b[${y + 1};${x + 1}H${if (line == 0) '-' else '|'}")
    @scala.annotation.tailrec def run(x: Int, y: Int, dir: Int, lines: Map[(Int, Int), Int]): Unit = {
      val (_dir, _lines) = if (lines.contains(x, y)) {
        val p = (dir, lines(x, y))
        (if (p == (0, 0) || p == (2, 1)) 1 else if (p == (0, 1) || p == (2, 0)) 3 else if (p == (1, 0) || p == (3, 1)) 0 else 2, lines - ((x, y)))
      } else (dir, lines)
      val (__x, __y, __dir) = (x + (if (_dir == 0 | _dir == 1) 1 else -1), y + (if (_dir == 1 | _dir == 2) 1 else -1)) match {
        case (-1, -1) => (1, 1, 1)
        case (-1, `ysize`) => (1, ysize - 2, 0)
        case (`xsize`, -1) => (xsize - 2, 1, 2)
        case (`xsize`, `ysize`) => (xsize - 2, ysize - 2, 3)
        case (-1, _y) => (1, _y, if (_dir == 2) 1 else 0)
        case (`xsize`, _y) => (xsize - 2, _y, if (_dir == 0) 3 else 2)
        case (_x, -1) => (_x, 1, if (_dir == 0) 1 else 2)
        case (_x, `ysize`) => (_x, ysize - 2, if (_dir == 1) 0 else 3)
        case (_x, _y) => (_x, _y, _dir)
      }
      (print(s"\u001b[${y + 1};${x + 1}H*"), Thread.sleep(50), print(s"\u001b[${y + 1};${x + 1}H "))
      run(__x, __y, __dir, _lines)
    }
    run(rand(xsize), rand(ysize), rand(4), lines)
  }
}
