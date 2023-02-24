object NumberSystems extends App {

  def toDecimal(num: Int, base: Int): Int = {
    num.toString.toList.view.reverse
      .map(_.asDigit)
      .zipWithIndex
      .map { case (digit, pow) => digit * Math.pow(base, pow).toInt }
      .sum
  }

  println(
    LazyList
      .from(2)
      .find(base =>
        toDecimal(418, base) + toDecimal(765, base) == toDecimal(1284, base)
      )
  )
}
