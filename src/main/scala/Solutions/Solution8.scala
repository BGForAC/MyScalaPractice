package Solutions

object Solution8 {
  def minimumCost(m: Int, n: Int, horizontalCut: Array[Int], verticalCut: Array[Int]): Int = {
    def f(horizontalSum: Int, verticalSum: Int, res: Int): Int = {
      if (horizontalSum == 0) verticalSum + res
      else if (verticalSum == 0) horizontalSum + res
      else {
        val ((hMax, hIndex), (vMax, vIndex)) = (horizontalCut.zipWithIndex.maxBy(_._1), verticalCut.zipWithIndex.maxBy(_._1))
        if (horizontalSum >= verticalSum) {
          horizontalCut(hIndex) = 0
          f(horizontalSum - hMax, verticalSum, res + hMax + verticalSum)
        }
        else {
          verticalCut(vIndex) = 0
          f(horizontalSum, verticalSum - vMax, res + vMax + horizontalSum)
        }
      }
    }

    f(horizontalCut.sum, verticalCut.sum, 0)
  }
}
