
object ThreeSum extends App {
import scala.collection.immutable.TreeSet
  def threeSum(nums: Array[Int]): List[List[Int]] = {
    if (nums.isEmpty) return List()
    val sortedNums = nums.sorted
    def loop(left: Int, right: Int, target: Int): List[List[Int]] = {
      if (left >= right) return Nil
      val leftVal = sortedNums(left)
      val rightVal = sortedNums(right)
      val sum = leftVal + rightVal
      lazy val nextLeft = (left + 1 until sortedNums.length).view
        .dropWhile(sortedNums(_) == leftVal)
        .headOption
        .getOrElse(sortedNums.length)
      lazy val nextRight = (right - 1 to 0 by -1).view
        .dropWhile((sortedNums(_) == rightVal))
        .headOption
        .getOrElse(-1)
      sum - target match {
        case 0          => List(leftVal, rightVal) :: loop(nextLeft, nextRight, target)
        case x if x < 0 => loop(nextLeft, right, target)
        case x          => loop(left, nextRight, target)
      }
    }

    (0 until sortedNums.length - 2)
      .distinctBy(sortedNums(_))
      .flatMap(i =>
        loop(i + 1, sortedNums.length - 1, 0 - sortedNums(i))
          .map(sortedNums(i) :: _)
      )
      .toList
  }

  println(threeSum(Array()))
}
