import scala.annotation.tailrec
import scala.util.Random.{nextInt => rand}

abstract class Line(val glyph: Char)

case object Horizontal extends Line('-')
case object Vertical extends Line('|')

object Line {
  def apply(n: Int): Line = if (n == 0) Horizontal else Vertical
}

trait Direction

case object RightUp extends Direction
case object RightDown extends Direction
case object LeftDown extends Direction
case object LeftUp extends Direction

object Direction {
  def apply(n: Int): Direction = n match {
    case 0 => RightUp
    case 1 => RightDown
    case 2 => LeftDown
    case _ => LeftUp
  }
}

object Bouncier {
  def main(args: Array[String]): Unit = {
    val xsize = args(0).toInt
    val ysize = args(1).toInt

    erase()
    val lines = (for (_ <- 1 to 50) yield generate(xsize, ysize)).flatten.toMap
    for (((x, y), line) <- lines) draw(x, y, line.glyph)

    @tailrec def run(x: Int, y: Int, dir: Direction, lines: Map[(Int, Int), Line]): Unit = {
      // Detect collision with line barrier and adjust position and direction.
      val (_dir, _lines) = lines.get(x, y) match {
        case Some(l) =>
          val _dir = (dir, l) match {
            case (RightUp, Horizontal) | (LeftDown, Vertical) => RightDown
            case (RightUp, Vertical) | (LeftDown, Horizontal) => LeftUp
            case (RightDown, Horizontal) | (LeftUp, Vertical) => RightUp
            case (RightDown, Vertical) | (LeftUp, Horizontal) => LeftDown
          }
          (_dir, lines - ((x, y)))
        case None =>
          (dir, lines)
      }

      // Move ball to next position.
      val (_x, _y) = _dir match {
        case RightUp => (x + 1, y - 1)
        case RightDown => (x + 1, y + 1)
        case LeftDown => (x - 1, y + 1)
        case LeftUp => (x - 1, y - 1)
      }

      // Detect ball collision with edge and adjust position and direction.
      val (__x, __y, __dir) = (_x, _y) match {
        // top-left corner
        case (-1, -1) => (1, 1, RightDown)
        // bottom-left corner
        case (-1, `ysize`) => (1, ysize - 2, RightUp)
        // top-right corner
        case (`xsize`, -1) => (xsize - 2, 1, LeftDown)
        // bottom-right corner
        case (`xsize`, `ysize`) => (xsize - 2, ysize - 2, LeftUp)
        // left edge
        case (-1, _y) => (1, _y, if (_dir == LeftDown) RightDown else RightUp)
        // right edge
        case (`xsize`, _y) => (xsize - 2, _y, if (_dir == RightUp) LeftUp else LeftDown)
        // top edge
        case (_x, -1) => (_x, 1, if (_dir == RightUp) RightDown else LeftDown)
        // bottom edge
        case (_x, `ysize`) => (_x, ysize - 2, if (_dir == RightDown) RightUp else LeftUp)
        // no collision
        case _ => (_x, _y, _dir)
      }

      draw(x, y, '*')
      Thread.sleep(50)
      clear(x, y)
      run(__x, __y, __dir, _lines)
    }
    run(rand(xsize), rand(ysize), Direction(rand(4)), lines)
  }

  def generate(xsize: Int, ysize: Int): Seq[((Int, Int), Line)] = {
    val x = rand(xsize / 2)
    val y = rand(ysize / 2)
    Line(rand(2)) match {
      case Horizontal => for (_x <- x until x + rand(xsize - x)) yield (_x, y) -> Horizontal
      case Vertical => for (_y <- y until y + rand(ysize - y)) yield (x, _y) -> Vertical
    }
  }

  def erase() = print(s"\u001b[2J\u001b[0;0H")

  def draw(x: Int, y: Int, c: Char) = print(s"\u001b[${y + 1};${x + 1}H${c}")

  def clear(x: Int, y: Int) = print(s"\u001b[${y + 1};${x + 1}H ")
}
