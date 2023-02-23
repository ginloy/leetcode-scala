import scala.annotation.tailrec

object LongestCommonPrefix extends App {
  def longestCommonPrefix(strs: Array[String]): String = {
    @tailrec
    def helper(strs: List[String]): String = {
      strs match {
        case Nil => ""
        case str :: Nil => str
        case str1 :: str2 :: xs => helper(commonPrefix(str1, str2) :: xs)
      }
    }
    helper(strs.toList)
  }

  def commonPrefix(str1: String, str2: String): String = {
    def helper(i: Int, j: Int): List[Char] = {
      if (i >= str1.length || j >= str2.length) return Nil
      if (str1(i) != str2(j)) return Nil
      str1(i) :: helper(i + 1, j + 1)
    }
    helper(0, 0).mkString
  }
}
