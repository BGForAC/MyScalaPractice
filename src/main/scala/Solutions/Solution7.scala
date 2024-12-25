package Solutions

object Solution7 {
  def eatenApples(apples: Array[Int], days: Array[Int]): Int = {
    case class DayApple(time: Int, var count: Int) extends Ordered[DayApple] {
      override def compare(that: DayApple): Int = that.time - this.time
    }

    val dayApples = apples.indices.map(i => DayApple(days(i) + i, apples(i)))

    val queue = scala.collection.mutable.PriorityQueue[DayApple]()
    queue.enqueue(dayApples(0))

    @scala.annotation.tailrec
    def f(day: Int, res: Int): Int = {
      if (queue.isEmpty) {
        if (day < apples.length) {
          queue.enqueue(dayApples(day))
          f(day + 1, res)
        } else res
      } else {
        val top = queue.head
        if (top.time < day || top.count == 0) {
          queue.dequeue()
          f(day, res)
        } else {
          top.count -= 1
          if (day < apples.length) queue.enqueue(dayApples(day))
          f(day + 1, res + 1)
        }
      }
    }

    f(1, 0)

  }
}