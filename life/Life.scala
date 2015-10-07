import scala.annotation.tailrec
import scala.util.Random.{nextInt => rand}
object Life {
  type Point = (Int, Int)
  def main(args: Array[String]): Unit = {
    val (xsize, ysize) = (args(0).toInt, args(1).toInt)
    @tailrec def run(universe: Universe): Unit = {
      print("\u001b[2J\u001b[0;0H" + (for (y <- 0 until ysize) yield (for (x <- 0 until xsize) yield if (universe.alive(x, y)) '#' else ' ').mkString + "\n").mkString)
      Thread.sleep(100)
      run(universe.tick)
    }
    val alive = (for (_ <- 0 until xsize * ysize / 4) yield (rand(xsize), rand(ysize))).toSet
    run(new Universe(alive)({ case (x, y) => x >= 0 && x < xsize && y >= 0 && y < ysize }))
  }
  class Universe(prior: Set[Point], val died: Set[Point], val born: Set[Point])(implicit boundary: Point => Boolean) {
    def this(prior: Set[Point])(implicit boundary: Point => Boolean) = this(prior, Set.empty, Set.empty)
    val alive = prior -- died ++ born
    def tick: Universe = new Universe(alive,
      alive filter { p => val l = p.living; l.size < 2 || l.size > 3 },
      alive flatMap { _.dead } filter { _.living.size == 3 })
    implicit class Cell(point: Point) {
      val (x, y) = point
      val neighbors = Set((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x + 1, y + 1), (x - 1, y + 1), (x + 1, y - 1), (x - 1, y - 1)) filter { boundary(_) }
      lazy val living = neighbors filter { alive(_) }
      lazy val dead = neighbors filter { !alive(_) }
    }
  }
}
