import scala.collection.immutable.TreeSet

object RemoveDuplicatesFromSortedArray extends App {
  def removeDuplicates(nums: Array[Int]): Int = {
    var i = 0
    while (i < nums.size) {
      var j = i + 1
      while(j < nums.length && nums(j) <= nums(i)) {
        j += 1
      }
      if (j == nums.length) return i + 1
      nums(i + 1) = nums(j)
      i += 1
    }
    i
  }
  val arr = Array(0,0,1,1,1,2,2,3,3,4)
  val cnt = removeDuplicates(arr)
  println(arr.mkString(", "), cnt)
}
