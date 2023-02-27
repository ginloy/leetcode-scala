object FindIndexOfFirstOccurrenceInString extends App {
  import scala.annotation.tailrec
  def strStr(haystack: String, needle: String): Int = {
    @tailrec
    def helper(i: Int): Int = {
      if (i + needle.length > haystack.length) return -1
      if (haystack.substring(i, i + needle.length).equals(needle)) return i
      helper(i + 1)
    }
    helper(0)
  }

}
