object letterCombinations extends App {
  def letterCombinations(digits: String): List[String] = {
    val map: Map[Char, String] = Map(
      '2' -> "abc",
      '3' -> "def",
      '4' -> "ghi",
      '5' -> "jkl",
      '6' -> "mno",
      '7' -> "pqrs",
      '8' -> "tuv",
      '9' -> "wxyz",
    )
    def helper(digits: List[Char]): List[String] = {
      digits match {
        case Nil => Nil
        case d :: Nil => map(d).toList.map(_.toString)
        case d :: xs => map(d).toList.flatMap(c => helper(xs).map(s => c + s))
      }
    }
    helper(digits.toList)
  }
  println(letterCombinations("7979"))
}

