object SearchInsertPosition extends App {
  def searchInsert(nums: Array[Int], target: Int): Int = {
    def bSearch(left: Int, right: Int): Int = {
      if (left >= right) return left
      val mid = left + (right - left) / 2
      val midVal = nums(mid)
      target.compare(midVal) match {
        case x if x == 0 => mid
        case x if x < 0 => bSearch(left, mid)
        case _ => bSearch(mid + 1 , right)
      }
    }
    bSearch(0, nums.length)
  }
}
