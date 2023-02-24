object RomanToInt extends App {
  def romanToInt(s: String): Int = {
    s match {
      case s"IX$rest" => 9 + romanToInt(rest)
      case s"IV$rest" => 4 + romanToInt(rest)
      case s"XL$rest" => 40 + romanToInt(rest)
      case s"XC$rest" => 90 + romanToInt(rest)
      case s"CD$rest" => 400 + romanToInt(rest)
      case s"CM$rest" => 900 + romanToInt(rest)
      case s"I$rest" => 1 + romanToInt(rest)
      case s"V$rest" => 5 + romanToInt(rest)
      case s"X$rest" => 10 + romanToInt(rest)
      case s"L$rest" => 50 + romanToInt(rest)
      case s"C$rest" => 100 + romanToInt(rest)
      case s"D$rest" => 500 + romanToInt(rest)
      case s"M$rest" => 1000 + romanToInt(rest)
      case _ => 0
    }
  }
}
