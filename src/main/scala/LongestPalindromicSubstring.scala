import scala.annotation.tailrec

object LongestPalindromicSubstring extends App {
  def longestPalindrome1(s: String): String = {
    def isPalindrome(left: Int, right: Int): Boolean = {
      if (left >= right) return true
      if (s(left) != s(right)) return false
      isPalindrome(left + 1, right - 1)
    }

    def helper(left: Int, right: Int, memo: Set[(Int, Int)]): (Int, Int, Set[(Int, Int)]) = {
      if (memo.contains((left, right))) return (0, 0, memo)
      val memo1: Set[(Int, Int)] = memo + ((left, right))
      if (isPalindrome(left, right)) return (left, right, memo1)
      val (left1, right1, memo2) = helper(left, right - 1, memo1)
      val (left2, right2, memo3) = helper(left + 1, right, memo2)
      if (right1 - left1 > right2 - left2) (left1, right1, memo3) else (left2, right2, memo3)
    }

    val (l, r, _) = helper(0, s.length - 1, Set())
    s.substring(l, r + 1)
  }

  def longestPalindrome(s: String): String = {
    @tailrec
    def helper(left: Int, right: Int): Option[(Int, Int)] = {
      if (s(left) != s(right)) return Option.empty
      val nextLeft = left - 1
      val nextRight = right + 1
      if (nextLeft < 0 || nextRight >= s.length) return Option((left, right))
      if (s(nextLeft) != s(nextRight)) return Option((left, right))
      helper(nextLeft, nextRight)
    }

    (0 until s.length - 1).view
      .flatMap(i => LazyList(helper(i, i), helper(i, i + 1)))
      .flatten
      .maxByOption(x => x._2 - x._1)
      .map { case (x, y) => s.substring(x, y + 1) }
      .getOrElse(s)
  }

  println(longestPalindrome1("civilwartestingwhetherthatnaptionoranynartionsoconceivedandsodedicatedcanlongendureWeareqmetonagreatbattlefiemldoftzhatwarWehavecometodedicpateaportionofthatfieldasafinalrestingplaceforthosewhoheregavetheirlivesthatthatnationmightliveItisaltogetherfangandproperthatweshoulddothisButinalargersensewecannotdedicatewecannotconsecratewecannothallowthisgroundThebravelmenlivinganddeadwhostruggledherehaveconsecrateditfaraboveourpoorponwertoaddordetractTgheworldadswfilllittlenotlenorlongrememberwhatwesayherebutitcanneverforgetwhattheydidhereItisforusthelivingrathertobededicatedheretotheulnfinishedworkwhichtheywhofoughtherehavethusfarsonoblyadvancedItisratherforustobeherededicatedtothegreattdafskremainingbeforeusthatfromthesehonoreddeadwetakeincreaseddevotiontothatcauseforwhichtheygavethelastpfullmeasureofdevotionthatweherehighlyresolvethatthesedeadshallnothavediedinvainthatthisnationunsderGodshallhaveanewbirthoffreedomandthatgovernmentofthepeoplebythepeopleforthepeopleshallnotperishfromtheearth"))
}
