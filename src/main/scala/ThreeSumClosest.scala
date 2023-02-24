object ThreeSumClosest extends App {
  import scala.annotation.tailrec
  def threeSumClosest(nums: Array[Int], target: Int): Int = {
    if (nums.length < 3) return 0
    val sortedNums = nums.sorted

    @tailrec
    def loop(
        left: Int,
        right: Int,
        target: Int,
        currentSum: Option[Int]
    ): Int = {
      if (left >= right) return currentSum.getOrElse(Integer.MIN_VALUE)
      val leftVal = sortedNums(left)
      val rightVal = sortedNums(right)
      val sum = leftVal + rightVal
      val nextSum = currentSum match {
        case None => Some(sum)
        case Some(prevSum) =>
          if ((sum - target).abs < (prevSum - target).abs) Some(sum)
          else currentSum
      }

      sum - target match {
        case 0          => sum
        case x if x < 0 => loop(left + 1, right, target, nextSum)
        case x          => loop(left, right - 1, target, nextSum)
      }
    }

    (0 until sortedNums.length - 2)
      .map(i =>
        loop(
          i + 1,
          sortedNums.length - 1,
          target - sortedNums(i),
          None
        ) + sortedNums(i)
      )
      .minBy(sum => (sum - target).abs)
  }

  println(threeSumClosest(Array(1, 1, 1, 1), 0))
}
