object RemoveNthNodeFromEndOfList extends App {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }
  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    def helper(node: ListNode): Int = {
        if (node.next == null) return 1
        val prev = helper(node.next)
        if (prev == n) {
          node.next = node.next.next
        }
        prev + 1
    }
    val prev = helper(head)
    if (prev == n && n == 1) return null
    if (prev == n) return head.next
    head
  }
}
