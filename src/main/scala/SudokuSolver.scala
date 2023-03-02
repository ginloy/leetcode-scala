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
        } board(row)(col) = solved.get(row, col).getOrElse('.')
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
          (1 to 9).view
            .map(d => set(i, j, d.toString.head))
            .filter(b => b.checkCell(i, j) && b.checkRow(i) && b.checkCol(j))
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
    def checkRow(row: Int): Boolean = {
      val lst = (0 until 9).view.flatMap(board.get(row, _))
      lst.size == lst.toSet.size
    }

    def checkCol(col: Int): Boolean = {
      val lst = (0 until 9).view.flatMap(board.get(_, col))
      lst.size == lst.toSet.size
    }
    def checkCell(row: Int, col: Int): Boolean = {
      val cellRow = row / 3
      val cellCol = col / 3
      val rowNums = (cellRow * 3 until cellRow * 3 + 3).view
      val colNums = (cellCol * 3 until cellCol * 3 + 3).view
      val chars = for {
        i <- rowNums
        j <- colNums
        c <- board.get((i, j))
      } yield c
      chars.size == chars.toSet.size
    }
  }
  println(Sudoku(Map()).solve)
}
