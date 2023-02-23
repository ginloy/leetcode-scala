object ZigzagConversion extends App {
  sealed trait Direction {}
  case object Up extends Direction
  case object Down extends Direction

  def convert(s: String, numRows: Int): String = {
    if (numRows == 1) return s
    val colSep = numRows - 2
    val segSize = numRows + colSep
    def helper(i: Int): (Int, Int) = {
      val seg = i / segSize
      val segIdx = i % segSize
      val row =
        if (segIdx / numRows == 0) segIdx else numRows - 2 - segIdx % numRows
      val segCol = if (segIdx / numRows == 0) 0 else 1 + segIdx % numRows
      val col = segCol + seg * (colSep + 1)
      (row, col)
    }
    (0 until s.length).view
      .map(i => { val (x, y) = helper(i); (x, y, s(i)) })
      .sorted
      .map { case (_, _, c) => c }
      .mkString
  }

  println(convert("PAYPALISHIRING", 2))
}
