import scala.annotation.tailrec
import scala.collection.LazyZip2

object GenerateParentheses extends App {
  def generateParenthesis(n: Int): List[String] = {
    val buffer = (n, n)
      def helper(pairs : Int, open: Int, accum: List[List[Char]]): List[List[Char]] = {
        (pairs, open) match {
          case (0, 0) => accum
          case (0, x) => helper(0, x - 1, accum.map(')' :: _))
          case (x, 0) => helper(x -1, 1, accum.map('(' :: _))
          case (x, y) => helper(x - 1, y + 1, accum.map('(' :: _)) ++ helper(x, y - 1, accum.map(')' :: _))
        }
      }
    helper(n, 0, List(List())).map(_.reverse.mkString)
  }

  println(generateParenthesis(8))
}
