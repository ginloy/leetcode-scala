import scala.annotation.tailrec
import scala.collection.immutable.IntMap

object TwoSum extends App {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    @tailrec
    def loop(idx: Int, idxMap: Map[Int, Int]): Array[Int] = {
      val num = nums(idx)
      val diff = target - num
      idxMap.get(diff) match {
        case None => loop(idx + 1, idxMap + (num -> idx))
        case Some(idx2) => Array(idx2, idx)
      }
    }
    loop(0, IntMap.empty)
  }

  println(twoSum(Array(2, 7, 11, 15), 9).mkString(","))
}
