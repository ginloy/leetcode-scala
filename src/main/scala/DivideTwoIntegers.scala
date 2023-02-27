object DivideTwoIntegers extends App {
  def divide(dividend: Int, divisor: Int): Int = {
    def helper(dividend: Long, divisor: Long): Long = {
      if (dividend < 0 && divisor < 0) return helper(-dividend, -divisor)
      if (dividend < 0) return -helper(-dividend, divisor)
      if (divisor < 0) return -helper(dividend, -divisor)
      loop(31, dividend, divisor)
    }
    def loop(bits: Int, dividend: Long, divisor: Long): Long = {
      if (bits == -1 || dividend < divisor) return 0
      if ((divisor << bits) <= dividend)
        return (1.toLong << bits) + loop(
          bits - 1,
          dividend - (divisor << bits),
          divisor
        )
      loop(bits - 1, dividend, divisor)
    }
    val res = helper(dividend.toLong, divisor.toLong)
    if (res < 0) res.max(Integer.MIN_VALUE).toInt
    res.min(Integer.MAX_VALUE).toInt
  }
  println(1 << 31)
  println(divide(-2147483648, -1))
}
