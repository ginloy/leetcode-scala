object LongestSubstrWithoutRepeat extends App {
  def lengthOfLongestSubstring(s: String): Int = {
    def loop(left: Int, current: Int, letterIdx: Map[Char, Int], currentMax: Int): Int = {
      val newMax = currentMax.max(current - left)
      if (current == s.length) return newMax
      val char = s.charAt(current)
//      if (letterIdx.contains(char)) {
//        val newLeft = letterIdx(char) + 1
//        val newLetterIdx = (left until newLeft).foldLeft(letterIdx)((m, i) => m.removed(char))
//        return loop(newLeft, current + 1, newLetterIdx + (char -> current), newMax)
//      }
//      loop(left, current + 1, letterIdx + (char -> current), newMax)
      (for {
        prev <- letterIdx.get(char)
        newLeft = prev + 1
        newLetterIdx = (left until newLeft).foldLeft(letterIdx)((m, i) => m.removed(s.charAt(i)))
      } yield loop(newLeft, current + 1 ,newLetterIdx + (char -> current), newMax))
        .getOrElse(loop(left, current + 1, letterIdx + (char -> current), newMax))
    }
    loop(0, 0, Map.empty, 0)
  }
  println(lengthOfLongestSubstring("nfpdmpi"))
}
