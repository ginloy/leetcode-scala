object FindFirstAndLastPositionOfElementInSortedArray extends App {
  def searchRange(nums: Array[Int], target: Int): Array[Int] = {
    def indexOfLeft(left: Int, right: Int): Option[Int] = {
      if (left >= right) return None
      val mid = left + (right - left) / 2
      val midVal = nums(mid)
      if (target == midVal) return indexOfLeft(left, mid).orElse(Some(mid))
      if (target < midVal) return indexOfLeft(left, mid)
      indexOfLeft(mid + 1, right)
    }

    def indexOfRight(left: Int, right: Int): Option[Int] = {
      if (left >= right) return None
      val mid = left + (right - left) / 2
      val midVal = nums(mid)
      if (target == midVal)
        return indexOfRight(mid + 1, right).orElse(Some(mid))
      if (target > midVal) return indexOfRight(mid + 1, right)
      indexOfRight(left, mid)
    }
    Array(
      indexOfLeft(0, nums.length).getOrElse(-1),
      indexOfRight(0, nums.length).getOrElse(-1)
    )
  }
  println(searchRange(Array(5, 7, 7, 8, 8, 10), 10).mkString(" "))

}
