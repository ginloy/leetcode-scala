object SwapNodesInPairs extends App{
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
      case Nil => null
      case x :: xs => new ListNode(x, fromList(xs))
    }
  }

  def swapPairs(head: ListNode): ListNode = {
    def helper(lst: List[Int]): List[Int] = {
      lst match {
        case Nil => Nil
        case x :: Nil => lst
        case x :: y :: xs => y :: x :: helper(xs)
      }
    }
    fromList(helper(toList(head)))
  }
}
