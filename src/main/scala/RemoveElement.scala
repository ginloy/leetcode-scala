object RemoveElement extends App {
  def removeElement(nums: Array[Int], `val`: Int): Int = {
    var i = 0
    while (i < nums.length) {
      if (nums(i) == `val`) {
        var j = i + 1
        while (j < nums.length && nums(j) == `val`) {
          j += 1
        }
        if (j == nums.length) return i
        nums(i) = nums(j)
        nums(j) = `val`
      }
      i+=1
    }
    i
  }
}
