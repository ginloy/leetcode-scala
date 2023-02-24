import scala.annotation.tailrec

object MergeKSortedLists extends App {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def toList(lstNode: ListNode): List[Int] = {
    if (lstNode == null) return Nil
    lstNode.x :: toList(lstNode.next)
  }

  def fromList(lst: List[Int]): ListNode = {
    lst match {
      case Nil     => null
      case x :: xs => new ListNode(x, fromList(xs))
    }
  }

  def merge(lst1: List[Int], lst2: List[Int]): List[Int] = {
    (lst1, lst2) match {
      case (Nil, _) => lst2
      case (_, Nil) => lst1
      case (x :: xs, y :: ys) =>
        if (x < y) x :: merge(xs, lst2)
        else y :: merge(lst1, ys)
    }
  }
  def mergeKLists(lists: Array[ListNode]): ListNode = {
    def loop(lsts: List[List[Int]]): List[List[Int]] = {
      lsts match {
        case Nil                => Nil
        case lst :: Nil         => lst :: Nil
        case lst1 :: lst2 :: xs => merge(lst1, lst2) :: loop(xs)
      }
    }
    @tailrec
    def helper(lsts: List[List[Int]]): List[Int] = {
      lsts match {
        case Nil        => Nil
        case lst :: Nil => lst
        case _          => helper(loop(lsts))
      }
    }
    fromList(helper(lists.toList.map(toList)))
  }
}
