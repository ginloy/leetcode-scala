object SudokuSolver extends App {
  def solveSudoku(board: Array[Array[Char]]): Unit = {
    val map = (for {
      row <- (0 until 9).view
      col <- (0 until 9).view
      digit = board(row)(col)
      if digit != '.'
    } yield ((row, col), digit)).toMap

    Sudoku(map).solve match {
      case None => return
      case Some(solved) =>
        for {
          row <- 0 until 9
          col <- 0 until 9
        } { board(row)(col) = solved.get(row, col).getOrElse('.') }
    }
  }
  case class Sudoku(private val board: Map[(Int, Int), Char]) {
    def get(i: Int, j: Int): Option[Char] = {
      board.get((i, j))
    }
    def set(i: Int, j: Int, c: Char): Sudoku = {
      Sudoku(board + ((i, j) -> c))
    }

    def solve: Option[Sudoku] = {
      findEmpty match {
        case None => Some(this)
        case Some((i, j)) =>
          ('1' to '9').view
            .filter(d => checkRow(i, d) && checkCol(j, d) && checkCell(i, j, d))
            .map(set(i, j, _))
            .flatMap(_.solve)
            .headOption
      }
    }

    def findEmpty: Option[(Int, Int)] = {
      (for {
        i <- (0 until 9).view
        j <- (0 until 9).view
        if !board.contains((i, j))
      } yield (i, j)).headOption
    }
    def checkRow(row: Int, digit: Char): Boolean = {
      (0 until 9).view.flatMap(board.get(row, _)).forall(_ != digit)
    }

    def checkCol(col: Int, digit: Char): Boolean = {
      (0 until 9).view.flatMap(board.get(_, col)).forall(_ != digit)
    }
    def checkCell(row: Int, col: Int, digit: Char): Boolean = {
      val cellRow = row / 3
      val cellCol = col / 3
      val rowNums = (cellRow * 3 until cellRow * 3 + 3).view
      val colNums = (cellCol * 3 until cellCol * 3 + 3).view
      val chars = for {
        i <- rowNums
        j <- colNums
        c <- board.get((i, j))
      } yield c
      chars.forall(_ != digit)
    }
  }
  println(Sudoku(Map()).solve)
}
