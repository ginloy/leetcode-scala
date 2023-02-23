object IntToRoman extends App {
  import scala.collection.immutable.TreeMap
  def intToRoman(num: Int): String = {
    val map: TreeMap[Int, String] = TreeMap(
      1 -> "I",
      5 -> "V",
      10 -> "X",
      50 -> "L",
      100 -> "C",
      500 -> "D",
      1000 -> "M",
      4 -> "IV",
      9 -> "IX",
      40 -> "XL",
      90 ->"XC",
      400 -> "CD",
      900 -> "CM"
    )
    def helper(num: Int): List[String] = {
      val biggest = map.maxBefore(num + 1)
      biggest match {
        case None => Nil
        case Some((k, v)) => v :: helper(num - k)
      }
    }
    helper(num).mkString
  }
}
