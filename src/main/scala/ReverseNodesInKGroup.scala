object ReverseNodesInKGroup extends App {
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

  def reverseKGroup(head: ListNode, k: Int): ListNode = {
    def split(lst: List[Int], n: Int): List[List[Int]] = {
      lst match {
        case Nil => List(List())
        case x :: xs if n == 1 =>
          val prev = split(xs, k)
          List(x) :: prev
        case x :: xs if n != 1 =>
          val prev = split(xs, n - 1)
          (x :: prev.head) :: prev.tail
      }
    }
    val lst = toList(head)
    val splt = split(lst, k)
    val res: List[Int] =
      if (lst.size % k == 0) splt.flatMap(_.reverse)
      else splt.dropRight(1).flatMap(_.reverse) ++ splt.last
    fromList(res)
  }
}
