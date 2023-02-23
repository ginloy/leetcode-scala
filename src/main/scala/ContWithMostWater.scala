
object ContWithMostWater extends App {
import scala.annotation.tailrec
  def maxArea(height: Array[Int]): Int = {
    if (height.isEmpty) return 0
    @tailrec
    def helper(left: Int, right: Int, currentMax: Int): Int = {
      if (left >= right) return currentMax
      val newMax = currentMax.max(area(left, right))
      height(left) - height(right) match {
        case x if x < 0 => helper(left + 1, right, newMax)
        case x          => helper(left, right - 1, newMax)
      }
    }

    def area(i: Int, j: Int): Int = height(i).min(height(j)) * (j - i)

    helper(0, height.length - 1, 0)
  }

  println(maxArea(Array(1, 8, 6, 2, 5, 4, 8, 3, 7)))
}
