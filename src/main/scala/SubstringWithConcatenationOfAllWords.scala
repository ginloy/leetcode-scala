object SubstringWithConcatenationOfAllWords extends App {
  def findSubstring(s: String, words: Array[String]): List[Int] = {
    val len = words(0).length
    def valid(i: Int, wordSet: Map[String, Int]): Boolean = {
      if (wordSet.isEmpty) return true
      if (i + len > s.length) return false
      val string = s.substring(i, i + len)
      wordSet.get(string) match {
        case None => false
        case Some(x) =>
          if (x == 1) valid(i + len, wordSet - string)
          else valid(i + len, wordSet + (string -> (x - 1)))
      }
    }
    val wordSet = words
      .foldLeft(Map[String, Int]())((map, word) =>
        map + (word -> (map.getOrElse(word, 0) + 1))
      )
    (0 to s.length).filter(i => valid(i, wordSet)).toList
  }

  println(findSubstring("barfoothefoobarman", Array("foo", "bar")))
}
