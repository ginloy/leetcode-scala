object SearchInRotatedSortedArray extends App {
  def search(nums: Array[Int], target: Int): Int = {
    def binarySearch(left: Int, right: Int): Option[Int] = {
        if (left >= right) return None
      val mid = left + (right - left) / 2
      val leftVal = nums(left)
      val rightVal = nums(right - 1)
      val midVal = nums(mid)
      if (target == midVal) return Some(mid)
      if (target < midVal && leftVal <= target) return binarySearch(left, mid)
      if (target > midVal && rightVal >= target) return binarySearch(mid + 1, right)
      if (target < midVal) return binarySearch(mid + 1, right).orElse(binarySearch(left, mid))
      binarySearch(left, mid).orElse(binarySearch(mid + 1, right))
    }
      binarySearch(0, nums.length).getOrElse(-1)
  }
  println(search(Array(7, 8, 1, 2, 3, 4, 5, 6), 2))
}
