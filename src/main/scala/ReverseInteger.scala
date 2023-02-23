import scala.util.Try

object ReverseInteger extends App {
  def reverse(x: Int): Int = {
    val reversedStr = {
      if (x < 0) '-' +: x.toString.drop(1).reverse
      else x.toString.reverse
    }
    Try(reversedStr.toInt).getOrElse(0)
  }
  println(reverse(-123))
}
