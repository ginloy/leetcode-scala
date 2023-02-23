object RegExMatching extends App {
  import scala.annotation.tailrec
  def isMatch(s: String, p: String): Boolean = {

    var visited: Set[(Int, Int)] = Set()
    def dfs(i: Int, j: Int): Boolean = {
      if (visited.contains((i, j))) return false
      visited = visited + ((i, j))
      if (i >= s.length && j >= p.length) return true
      next(i, j)
        .exists { case (x, y) => dfs(x, y) }
    }

    def next(i: Int, j: Int): List[(Int, Int)] = {
      var res: List[(Int, Int)] = List()
      if (j < p.length - 1 && p.charAt(j + 1) == '*') {
        if (i < s.length && (s.charAt(i) == p.charAt(j) || p.charAt(j) == '.'))
          res = (i + 1, j) :: res
        res = (i, j + 2) :: res
      }
      if (
        i < s.length && j < p.length && (s.charAt(i) == p
          .charAt(j) || p.charAt(j) == '.')
      ) {
        res = (i + 1, j + 1) :: res
      }
      res
    }
    dfs(0, 0)
  }

  def compare(
      string: List[(Char, Int)],
      pattern: List[(Char, Int)],
      memo: Map[(Int, Int), Boolean]
  ): (Boolean, Map[(Int, Int), Boolean]) = {
    (string, pattern) match {
      case (Nil, Nil) => (true, memo)
      case ((_, i) :: _, (_, j) :: _) if memo.contains((i, j)) =>
        (memo((i, j)), memo)
      case ((x, i) :: xs, ((y, j) :: ('*', k) :: ys)) if x == y || y == '.' =>
        val (valid1, memo1) = compare(string, ys, memo)
        val (valid2, memo2) = compare(xs, pattern, memo1)
        if (valid1 || valid2) (true, memo2 + ((i, j) -> true))
        else (false, memo2 + ((i, j) -> false))
      case (string, (y, j) :: ('*', k) :: ys) =>
        compare(string, ys, memo)
      case ((x, i) :: xs, (y, j) :: ys) if x == y || y == '.' =>
        val (valid, memo1) = compare(xs, ys, memo)
        (valid, memo1 + ((i, j) -> valid))
      case _ => (false, memo)
    }
  }

  @tailrec
  def dropWhileN[T](lst: List[T], pred: T => Boolean, n: Int): List[T] = {
    if (n == 0) return lst
    lst match {
      case Nil                => Nil
      case x :: xs if pred(x) => dropWhileN(xs, pred, n - 1)
      case lst                => lst
    }
  }

  println(isMatch("ab", ".*c"))
}
