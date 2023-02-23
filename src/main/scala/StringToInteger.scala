import scala.annotation.tailrec
import scala.util.Try

object StringToInteger extends App {
  def myAtoi(s: String): Int = {
    @tailrec
    def loop(lst: List[Char], accum: Long, sign: Int): Int = {
      lst match {
        case _ if sign * accum < Integer.MIN_VALUE => Integer.MIN_VALUE
        case _ if sign * accum > Integer.MAX_VALUE => Integer.MAX_VALUE
        case x :: xs if x.isDigit => loop(xs, accum * 10 + x.asDigit, sign)
        case _ => (sign * accum).toInt
      }
    }

    s.trim.toList match {
      case '-' :: xs => loop(xs, 0, -1)
      case '+' :: xs => loop(xs, 0, 1)
      case str => loop(str, 0, 1)
    }
  }

  println(myAtoi("-154321523987538290760823"))
}
