package stackoverflow

object Helpers {
  def median_2(input: Array[(Int, Int)]) = {
    val arr = input.sortBy(_._2)
    val m = arr.length / 2
    if (arr.length % 2 == 0) (arr(m)._2 + arr(m - 1)._2) / 2 else arr(m)._2
  }
}
