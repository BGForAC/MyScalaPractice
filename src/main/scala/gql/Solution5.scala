package gql

import scala.collection.mutable

object Solution5 {
  def closestRoom(rooms: Array[Array[Int]], queries: Array[Array[Int]]): Array[Int] = {
    rooms.sortBy(-_(1))
    var arrBegin = 0
    val res = Array.fill(queries.length)(0)
    val arrayBuffer = mutable.TreeSet[Int]()
    queries.zipWithIndex.sortBy(-_._1(1)).foreach({
      case (arr, idx) => {
        res(idx) = {
          if (arrBegin != rooms.length) {
            val lastIndex = rooms.indexWhere(innerArr => innerArr(1) < arr(1))
            if (lastIndex != -1) {
              arrayBuffer ++= rooms.slice(arrBegin,lastIndex).map(_(0))
              arrBegin = lastIndex
            } else arrBegin = rooms.length
          }
          val isArrayEmpty = arrayBuffer.isEmpty
          if (isArrayEmpty) -1 else {
            val min = arrayBuffer.to(idx).last
            val max = arrayBuffer.from(idx).head
            if (max - arr(0) < arr(0) - min) max else min
          }
        }
      }
    })
    res
  }
}