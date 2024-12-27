package PathFindingAlgorithm

class JpsBitMap(grid: Array[Array[Byte]]) {
  final val BITSPERCELL = 32
  final val BLOCK = 0
  final val EMPTY = 1
  final val BLOCKCELL = 0x00000000
  final val EMPTYCELL = 0xffffffff
  private val rows = MapReader.rows / BITSPERCELL + 1
  private val cols = MapReader.cols / BITSPERCELL + 1
  private val map: Array[Array[Int]] = Array.ofDim[Int](MapReader.rows, cols)
  private val transposeMap: Array[Array[Int]] = Array.ofDim[Int](MapReader.cols, rows)
  private var mapSurroundedWithBlock: Array[Array[Int]] = Array.ofDim[Int](rows + 2, cols + 2)
  private var transposeMapSurroundedWithBlock: Array[Array[Int]] = Array.ofDim[Int](cols + 2, rows + 2)


  makeMap(grid)
  /**
   * 将一格一位的地图转换为一格32位的地图
   * @param array
   * @return
   */
  private def makeMap(array: Array[Array[Byte]]): Unit = {
    val totRows = array.length
    val totCols = array(0).length
    for (i <- 0 until totRows) {
      for (j <- 0 until cols) {
        var value = 0
        for (k <- 0 until BITSPERCELL) {
          val index = j * BITSPERCELL + k
          if (index < totCols) {
            value = value | (array(i)(index) << k)
          } else {
            value = value | (BLOCK << k)
          }
        }
        map(i)(j) = value
      }
    }
    mapSurroundedWithBlock = surroundedWithBlock(map)

    for (i <- 0 until totCols) {
      for (j <- 0 until rows) {
        var value = 0
        for (k <- 0 until BITSPERCELL) {
          val index = j * BITSPERCELL + k
          if (index < totRows) {
            value = value | (array(index)(i) << k)
          } else {
            value = value | (BLOCK << k)
          }
        }
        transposeMap(i)(j) = value
      }
    }
    transposeMapSurroundedWithBlock = surroundedWithBlock(transposeMap)
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

  private def getRowPos(row: Int): Int = row / BITSPERCELL

  def getRowOffset(row: Int): Int = row % BITSPERCELL

  private def getColumnPos(col: Int): Int = col / BITSPERCELL

  def getColumnOffset(col: Int): Int = col % BITSPERCELL

  //低位取1，高位不变
  private def maskedByOffset(value: Int, offset: Int): Int = value | ~(0xffffffff << offset)

  def getCells(pos: (Int, Int), dir: (Int, Int)): Int = {
    if (dir._1 == 0 && dir._2 > 0){
      mapSurroundedWithBlock(pos._1 + 1)(getColumnPos(pos._2) + 1)
    } else if (dir._1 == 0 && dir._2 < 0){
      val res = mapSurroundedWithBlock(pos._1 + 1)(getColumnPos(pos._2) + 1)
      //按位倒序
      Integer.reverse(res)
    } else if (dir._1 > 0 && dir._2 == 0){
      transposeMapSurroundedWithBlock(pos._2 + 1)(getRowPos(pos._1) + 1)
    } else {
      val res = transposeMapSurroundedWithBlock(pos._2 + 1)(getRowPos(pos._1) + 1)
      //按位倒序
      Integer.reverse(res)
    }
  }

  def getMaskedCells(pos: (Int, Int), dir: (Int, Int), offset: Int): Int = {
    maskedByOffset(getCells(pos, dir), offset)
  }

}
