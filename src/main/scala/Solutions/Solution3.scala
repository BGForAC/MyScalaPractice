package Solutions

import scala.collection.mutable
import scala.math

object Solution3 {
  private final val MOD = 1e9.toInt + 7

  private case class Node(index: Int,var value: Long)

  private def pow(a: Long, b: Long): Long = {

    @scala.annotation.tailrec
    def powRec(a: Long, b: Long, res: Long): Long = {
      if (b == 0) res
      else {
        if (b % 2 == 0) powRec(a * a % MOD, b / 2, res)
        else powRec(a * a % MOD, b / 2, res * a % MOD)
      }
    }

    powRec(a, b, 1L)
  }

  def getFinalState(nums: Array[Int], k: Int, multiplier: Int): Array[Int] = {

    if (multiplier == 1) return nums

    val n = nums.length
    val res = Array.fill(n)(0)

    implicit val nodeOrdering: Ordering[Node] =
                math.Ordering.fromLessThan((a, b) => if (a.value != b.value) a.value < b.value else a.index < b.index)

    val pq = mutable.PriorityQueue.empty[Node](nodeOrdering.reverse)
    var maxVal = 0L

    nums.zipWithIndex.foreach{case (a, b) => {
      maxVal = maxVal max a.toLong
      pq.enqueue(Node(b, a.toLong))
    }}
    var remainingK = k

    while (remainingK > 0 && pq.head.value * multiplier <= maxVal) {
      remainingK -= 1
      val node = pq.dequeue()
      node.value = node.value * multiplier % MOD
      pq.enqueue(node)
    }

    val powerMultiplier = pow(multiplier, remainingK / n)
    var kRem = remainingK % n

    while (pq.nonEmpty) {
      val node = pq.dequeue()
      if (remainingK / n > 0) node.value = node.value * powerMultiplier % MOD
      if (kRem > 0) {
        kRem -= 1
        node.value = node.value * multiplier % MOD
      }
      res(node.index) = node.value.toInt
    }

    res
  }
}
