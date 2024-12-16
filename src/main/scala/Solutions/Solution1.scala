package Solutions

object Solution1 {
  def maxSpending(values: Array[Array[Int]]): Long = {
    values
      .flatten
      .sorted
      .zipWithIndex
      .map{ case (v, i) => v * (i + 1).longValue() }
      .sum
  }
}