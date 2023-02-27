object NextPermutation extends App {
  def nextPermutation(nums: Array[Int]): Unit = {
    def helper(i: Int): Unit = {
      if (i == -1) {
        nums.sortInPlace
        return
      }
      val j = (i until nums.length).view
        .filter(nums(_) > nums(i))
        .minByOption(nums(_))
      j match {
        case None => helper(i - 1)
        case Some(j) =>
          swap(nums, i, j)
          sort(nums, i + 1, nums.length)
      }
    }
    def swap[T](arr: Array[T], left: Int, right: Int): Unit = {
      val temp = arr(right)
      arr(right) = arr(left)
      arr(left) = temp
    }
    def sort(nums: Array[Int], left: Int, right: Int): Unit = {
      if (left >= right - 1) return
      val pivot = nums(left)
      var i = left
      var j = left + 1
      while (j < right) {
        if (nums(j) < pivot) {
          i += 1
          swap(nums, i, j)
        }
        j += 1
      }
      swap(nums, left, i)
      sort(nums, left, i)
      sort(nums, i + 1, right)
    }
    helper(nums.length - 1)
  }

  val nums = Array(3, 2, 1)
  println(nums.mkString(", "))
  nextPermutation(nums)
  println(nums.mkString(", "))

}
