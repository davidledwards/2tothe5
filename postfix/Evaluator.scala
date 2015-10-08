import scala.annotation.tailrec
object Evaluator {
  val ops: Map[String, (Int, Int) => Int] = Map(
    "+" -> { _ + _ },
    "-" -> { _ - _ },
    "*" -> { _ * _ },
    "/" -> { _ / _ }
  )
  def main(args: Array[String]): Unit = {
    @tailrec def eval(args: Array[String], stack: List[Int]): Option[Int] = args.headOption match {
      case Some(op) if ops.contains(op) => stack match {
        case r :: l :: _stack => eval(args.tail, ops(op)(l, r) :: _stack)
        case _ => None
      }
      case Some(arg) => (try Some(arg.toInt) catch {
        case _: NumberFormatException => None
      }) match {
        case Some(v) => eval(args.tail, v :: stack)
        case None => None
      }
      case None => stack match {
        case result :: Nil => Some(result)
        case _ => None
      }
    }
    eval(args, Nil) match {
      case Some(result) => println(result)
      case None => println("invalid expression")
    }
  }
}
