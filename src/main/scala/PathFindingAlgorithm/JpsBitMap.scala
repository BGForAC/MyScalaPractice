package PathFindingAlgorithm

class JpsBitMap(grid: Array[Array[Byte]]) {
  final val BITSPERCELL = 32
  final val BITOFFSET = powerOfTwo(BITSPERCELL)
  final val BITMASK = ~(0xffffffff << BITOFFSET)
  final val OBSTACLE = 0
  final val EMPTY = 1
  private final val OBSTACLECELL = 0x00000000
  private final val EMPTYCELL = 0xffffffff
  private val totRows = grid.length
  private val totCols = grid(0).length
  private val rows = (totRows >> BITOFFSET) + 1
  private val cols = (totCols >> BITOFFSET) + 1
  private val mapSurroundedWithObstacle: Array[Array[Int]] = Array.fill(totRows + 2, cols + 2)(OBSTACLECELL)
  private val mapSurroundedWithObstacleReverseBit: Array[Array[Int]] = Array.fill(totRows + 2, cols + 2)(OBSTACLECELL)
  private val transposeMapSurroundedWithObstacle: Array[Array[Int]] = Array.fill(totCols + 2, rows + 2)(OBSTACLECELL)
  private val transposeMapSurroundedWithObstacleReverseBit: Array[Array[Int]] = Array.fill(totCols + 2, rows + 2)(OBSTACLECELL)

  private def powerOfTwo(n: Int): Int = {
    require(n > 0 && (n & (n - 1)) == 0, "n must be a power of 2")
    new NativeCall().ffs(n) - 1
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
            value = value | (OBSTACLE << k)
          }
        }
        mapSurroundedWithObstacle(i + 1)(j + 1) = value
        mapSurroundedWithObstacleReverseBit(i + 1)(j + 1) = Integer.reverse(value)
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
            value = value | (OBSTACLE << k)
          }
        }
        transposeMapSurroundedWithObstacle(i + 1)(j + 1) = value
        transposeMapSurroundedWithObstacleReverseBit(i + 1)(j + 1) = Integer.reverse(value)
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
    grid.count(_.count(_ == 0) > 0)
  }

  private def surroundedWithObstacle(map: Array[Array[Int]]):Array[Array[Int]]  = {
    val rows = map.length
    val cols = map(0).length
    val newMap = Array.fill(rows + 2, cols + 2)(OBSTACLECELL)
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
    mapSurroundedWithObstacle(x + 1)(getColumnPos(y) + 1)
  }

  private def getPosInMapReverseBit(x: Int, y: Int) = {
    mapSurroundedWithObstacleReverseBit(x + 1)(getColumnPos(y) + 1)
  }

  private def getPosInTransposeMap(x: Int, y: Int) = {
    transposeMapSurroundedWithObstacle(y + 1)(getRowPos(x) + 1)
  }

  private def getPosInTransposeMapReverseBit(x: Int, y: Int) = {
    transposeMapSurroundedWithObstacleReverseBit(y + 1)(getRowPos(x) + 1)
  }

  //低位取1，高位不变
  private def maskedByOffset(value: Int, offset: Int): Int = value | ~(0xffffffff << offset)

  def getCells(x: Int, y: Int, dx: Int, dy: Int): Int = {
    if (dx == 0) {
      if (dy > 0) {
        getPosInMap(x, y)
      } else {
        getPosInMapReverseBit(x, y)
      }
    } else {
      if (dx > 0) {
        getPosInTransposeMap(x, y)
      } else {
        getPosInTransposeMapReverseBit(x, y)
      }
    }
  }

  def getMaskedCells(x: Int, y: Int, dx: Int, dy: Int, offset: Int): Int = {
    maskedByOffset(getCells(x, y, dx, dy), offset)
  }
}
