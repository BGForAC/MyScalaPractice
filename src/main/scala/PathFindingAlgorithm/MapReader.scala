package PathFindingAlgorithm

import java.io.{File, FileInputStream, IOException}
import scala.collection.mutable.ArrayBuffer

object MapReader {
  var rows = 0
  var cols = 0
  var offsetZ = 0
  var offsetX = 0
  var side_length = 0
//  val start = (5561, 13422)
//  val end = (-2333, -4172)
  val start = (4830, 13250)
  val end = (-7333, -6312)

  var map: Array[Array[Byte]] = _

  final val BLOCK = 0
  final val EMPTY = 1

  private def getBlockPost(pos: (Int, Int)): (Int, Int) = {
    val (x, y) = pos
    (getBlockRowPos(x), getBlockColPos(y))
  }

  private def getBlockRowPos(row: Int): Int = {
    (row - offsetX + side_length / 2) / side_length
  }

  private def getBlockColPos(col: Int): Int = {
    (col - offsetZ + side_length / 2) / side_length
  }

  private def bytesToInt(bytes: Array[Byte]) =
    (bytes(3) << 24 & 0xff000000) + (bytes(2) << 16 & 0xff0000) + (bytes(1) << 8 & 0xff00) + (bytes(0) & 0xff)

  def readMap(filePath: String): Array[Array[Byte]] = {
    val inputStream = new FileInputStream(filePath)
    val intBuffer = new Array[Byte](4)
    val seq = for {_ <- 0 until 5
                     _ = inputStream.read(intBuffer)
                     res = bytesToInt(intBuffer)} yield(res)
    val IndexedSeq(cols, rows, offsetZ, offsetX, side_length) = seq
    this.rows = rows
    this.cols = cols
    this.offsetZ = offsetZ
    this.offsetX = offsetX
    this.side_length = side_length
    val map = Array.ofDim[Byte](rows, cols)
    val mapBuffer = new Array[Byte](rows * cols / 8 + 1)
    inputStream.read(mapBuffer)
    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        val index = i * cols + j
        val byteIndex = index / 8
        val bitIndex = index % 8
        val byte = mapBuffer(byteIndex)
        val bit = (byte >> bitIndex) & 1
        map(i)(j) = bit.toByte
      }
    }
    map
  }

  def main(args: Array[String]): Unit = {
    System.loadLibrary("NativeCall")
    val filePath = "map36.bytes"
    this.map = readMap(filePath)
    val start = getBlockPost(this.start)
    val end = getBlockPost(this.end)
    jpsTestGroup1(start, end, map)
//    aStarTestGroup1(start, end, map)
  }

  private def jpsTestGroup1(start: (Int, Int), end: (Int, Int), map: Array[Array[Byte]]): Unit = {
    val iterCount = 1
    var path = List[Jps.Node]()
    val jps = new Jps(start, end, map, new JpsBitMap(map))

//    jps.jps()
//    jps.jps()
//    jps.jps()
//    jps.jps()
//    jps.jps()
//    jps.jps()
//    jps.jps()
//    jps.jps()
//    jps.jps()
//    jps.jps()

    println(s"start: $start, end: $end")

    val startTime = System.nanoTime()
    for (_ <- 0 until iterCount) path = jps.jps()
    val endTime = System.nanoTime()
    println(s"Time: ${(endTime - startTime) / 1e9} seconds")

    path.foreach(node => println(s"${node.x} ${node.y}"))
  }

  def aStarTestGroup1(start: (Int, Int), end: (Int, Int), map: Array[Array[Byte]]): Unit = {
    println(s"start: $start, end: $end")
    val emptyMap = Array.ofDim[Byte](rows, cols)
    val aStar = new AStar(start, end, map)
    val startTime = System.nanoTime()
    val path = aStar.aStar()
    val endTime = System.nanoTime()
    println(s"Time: ${(endTime - startTime) / 1e9} seconds")
    path.foreach(node => println(s"${node.x} ${node.y}"))
    //    path.foreach(node => println(s"${node.x}, ${node.y}"))

    //    val path1 = new AStar((35, 410), (73, 110), map).aStar()
    //    path1.foreach(node => println(s"${node.x}, ${node.y}"))
  }


}