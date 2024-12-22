package TestCodeExample

import scala.collection.mutable.ArrayBuffer

object KmpPractice {
  def getNextArray(str: String): Array[Int] = {
    var cur = -1
    val next = new Array[Int](str.length + 1)
    for ((c, index) <- str.zipWithIndex) {
      if (c == str(next(index))) cur += 1 else cur = 0
      next(index + 1) = cur
    }
    next
  }

  def kmpMatcher(str: String, subStr: String): IndexedSeq[Int] = {
    val next = getNextArray(subStr)
    next.foreach(println)

    @scala.annotation.tailrec
    def f(i: Int, j: Int, result: ArrayBuffer[Int]): Array[Int] = {
      if (j == subStr.length) {
        result += i - subStr.length
        f(i, next(j), result)
      } else if (i == str.length) {
        result.toArray
      }else if (subStr(j) == str(i)) {
        f(i + 1, j + 1, result)
      } else if (subStr(j) != str(i) && j != 0) {
        f(i, next(j), result)
      } else {
        f(i + 1, j, result)
      }
    }

    f(0, 0, ArrayBuffer.empty)
  }
}