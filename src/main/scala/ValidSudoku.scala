import scala.collection.View.DistinctBy

object ValidSudoku extends App {
  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    def checkRow(row: Int, col: Int): Boolean = {
      val lst = (0 until 9).view
        .map(board(row)(_))
        .filter(_!='.')
      lst.toSet.sizeCompare(lst) == 0
    }

    def checkCol(row: Int, col: Int): Boolean = {
      val lst = (0 until 9).view
        .map(board(_)(col))
        .filter(_!='.')
      lst.toSet.sizeCompare(lst) == 0
    }

    def checkSubGrid(row: Int, col: Int): Boolean = {
      val cellRow = row / 3
      val cellCol = col / 3
      val lst = for {
        i <- (cellRow * 3 until cellRow * 3 + 3).view
        j <- (cellCol * 3 until cellCol * 3 + 3).view
        if board(i)(j) != '.'
      } yield board(i)(j)
      lst.toSet.sizeCompare(lst) == 0
    }
    (for {
      row <- (0 until 9).view
      col <- (0 until 9).view
      if board(row)(col) != '.'
    } yield checkRow(row, col) && checkCol(row, col) && checkSubGrid(row, col))
      .find(_==false)
      .getOrElse(true)
  }
}
