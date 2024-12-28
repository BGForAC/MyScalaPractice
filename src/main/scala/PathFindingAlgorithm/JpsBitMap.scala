package PathFindingAlgorithm

import scala.annotation.tailrec

class JpsBitMap(grid: Array[Array[Byte]]) {
  final val BITSPERCELL = 32
  final val BITOFFSET = powerOfTwo(BITSPERCELL)
  final val BLOCK = 0
  final val EMPTY = 1
  final val BLOCKCELL = 0x00000000
  final val EMPTYCELL = 0xffffffff
  private val totRows = grid.length
  private val totCols = grid(0).length
  private val rows = (totRows >> BITOFFSET) + 1
  private val cols = (totCols >> BITOFFSET) + 1
  private val mapSurroundedWithBlock: Array[Array[Int]] = Array.fill(totRows + 2, cols + 2)(BLOCKCELL)
  private val mapSurroundedWithBlockReverseBit: Array[Array[Int]] = Array.fill(totRows + 2, cols + 2)(BLOCKCELL)
  private val transposeMapSurroundedWithBlock: Array[Array[Int]] = Array.fill(totCols + 2, rows + 2)(BLOCKCELL)
  private val transposeMapSurroundedWithBlockReverseBit: Array[Array[Int]] = Array.fill(totCols + 2, rows + 2)(BLOCKCELL)

  private def powerOfTwo(n: Int): Int = {
    require(n > 0 && (n & (n - 1)) == 0, "n must be a power of 2")
    var count = 0
    while (Math.pow(2, count).toInt != n) {
      count += 1
    }
    count
  }

  makeMap()
  /**
   * 将一格一位的地图转换为一格32位的地图
   * @return
   */
  private def makeMap(): Unit = {
    for (i <- 0 until totRows) {
      for (j <- 0 until cols) {
        var value = 0
        val begin = j << BITOFFSET
        for (k <- 0 until BITSPERCELL) {
          val index = begin + k
          if (index < totCols) {
            value = value | (grid(i)(index) << k)
          } else {
            value = value | (BLOCK << k)
          }
        }
        mapSurroundedWithBlock(i + 1)(j + 1) = value
        mapSurroundedWithBlockReverseBit(i + 1)(j + 1) = Integer.reverse(value)
      }
    }

    for (i <- 0 until totCols) {
      for (j <- 0 until rows) {
        var value = 0
        val begin = j << BITOFFSET
        for (k <- 0 until BITSPERCELL) {
          val index = begin + k
          if (index < totRows) {
            value = value | (grid(index)(i) << k)
          } else {
            value = value | (BLOCK << k)
          }
        }
        transposeMapSurroundedWithBlock(i + 1)(j + 1) = value
        transposeMapSurroundedWithBlockReverseBit(i + 1)(j + 1) = Integer.reverse(value)
      }
    }
  }

  def printMap(grid: Array[Array[Int]]): Unit = {
    for (i <- grid.indices) {
      for (j <- grid(0).indices) {
        print(grid(i)(j) + " ")
      }
      println()
    }
  }

  private def surroundedWithBlock(map: Array[Array[Int]]):Array[Array[Int]]  = {
    val rows = map.length
    val cols = map(0).length
    val newMap = Array.fill(rows + 2, cols + 2)(BLOCKCELL)
    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        newMap(i + 1)(j + 1) = map(i)(j)
      }
    }
    newMap
  }

  private def getRowPos(row: Int): Int = row >> BITOFFSET

  private def getColumnPos(col: Int): Int = col >> BITOFFSET

  private def getPosInMap(x: Int, y: Int) = {
    mapSurroundedWithBlock(x + 1)(getColumnPos(y) + 1)
  }

  private def getPosInMapReverseBit(x: Int, y: Int) = {
    mapSurroundedWithBlockReverseBit(x + 1)(getColumnPos(y) + 1)
  }

  private def getPosInTransposeMap(x: Int, y: Int) = {
    transposeMapSurroundedWithBlock(y + 1)(getRowPos(x) + 1)
  }

  private def getPosInTransposeMapReverseBit(x: Int, y: Int) = {
    transposeMapSurroundedWithBlockReverseBit(y + 1)(getRowPos(x) + 1)
  }

  //低位取1，高位不变
  private def maskedByOffset(value: Int, offset: Int): Int = value | ~(0xffffffff << offset)

  def getCells(pos: (Int, Int), dir: (Int, Int)): Int = {
    val dx = dir._1
    val dy = dir._2
    if (dx == 0 && dy > 0){
      getPosInMap(pos._1, pos._2)
    } else if (dx == 0 && dy < 0){
      getPosInMapReverseBit(pos._1, pos._2)
    } else if (dx > 0 && dy == 0){
      getPosInTransposeMap(pos._1, pos._2)
    } else {
      getPosInTransposeMapReverseBit(pos._1, pos._2)
    }
  }

  def getMaskedCells(pos: (Int, Int), dir: (Int, Int), offset: Int): Int = {
    maskedByOffset(getCells(pos, dir), offset)
  }
}
