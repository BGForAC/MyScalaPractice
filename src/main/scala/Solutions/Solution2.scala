package Solutions

object Solution2 {

  def getFinalState(nums: Array[Int], k: Int, multiplier: Int): Array[Int] = {
    for (i <- 1 to k) {
      val minIndex = nums.indexOf(nums.min)
      nums.update(minIndex, nums(minIndex) * multiplier)
    }
    nums
  }

}