object LongestValidParentheses extends App {
  def longestValidParentheses(s: String): Int = {
    def helperLeft(c: Char,tup: (Int, Int, Int)): (Int, Int, Int) = {
      val (left, right, mx) = tup
      c match {
        case '(' =>
          val newLeft = left + 1
          if (newLeft == right) (newLeft, right, mx.max(right * 2))
          else if (right > newLeft) (0, 0, mx)
          else (newLeft, right, mx)
        case ')' =>
          val newRight = right + 1
          if (left == newRight) (left, newRight, mx.max(left * 2))
          else if (newRight > left) (0, 0, mx)
          else (left, newRight, mx)
        case _ => (0, 0, Integer.MIN_VALUE)
      }
    }

    def helperRight(c: Char, tup: (Int, Int, Int)): (Int, Int, Int) = {
      val (left, right, mx) = tup
      c match {
        case '(' =>
          val newLeft = left + 1
          if (newLeft == right) (newLeft, right, mx.max(right * 2))
          else if (newLeft > right) (0, 0, mx)
          else (newLeft, right, mx)
        case ')' =>
          val newRight = right + 1
          if (left == newRight) (left, newRight, mx.max(left * 2))
          else if (left > newRight) (0, 0, mx)
          else (left, newRight, mx)
        case _ => (0, 0, Integer.MIN_VALUE)
      }
    }

    s.foldLeft((0, 0, 0)) { case (tup, c) => helperLeft(c, tup) }
      ._3
      .max(
        s.foldRight((0, 0, 0)) { case (c, tup) => helperRight(c, tup) }._3
      )
  }
  println(longestValidParentheses("()"))
}
