package PathFindingAlgorithm

import PathFindingAlgorithm.Jps.Node

import java.io.FileOutputStream
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn
import scala.language.implicitConversions

class Jps(start: (Int, Int), end: (Int, Int), grid: Array[Array[Byte]], jpsBitMap: JpsBitMap) {
  /**
   * 强迫邻居判断
   * 1 2 3
   * 4 5 6
   * 7 8 9
   * 强迫邻居在要判断跳点的周围的八个点（邻居）中，其中一点是父节点，八个点中有墙
   * 斜对角线从3到5。4，7，8是默认的不会被修剪邻居，只有墙在父节点相邻两点有墙且在0父节点的对面的点不为墙，才能说明5是跳点
   * 直线从6到5。4是默认不会被修剪的邻居，2，8为墙且在父节点前进方向不为墙则有5是跳点
   * 强迫邻居要能改变父节点搜索路径
   */
  private final val nativeCallObj = new NativeCall()

  private final val BLOCK = 0
  private final val EMPTY = 1
  private final val StraightMoveCost = 10
  private final val DiagonalMoveCost = 14

  private val closed = mutable.HashSet[(Int, Int)]()
  private val open = mutable.PriorityQueue[Node]()

  private def surroundedWithOnes(grid: Array[Array[Byte]]): Array[Array[Byte]] = {
    val rows = grid.length
    val cols = grid(0).length
    val newGrid: Array[Array[Byte]] = Array.fill(rows + 2, cols + 2)(1)

    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        newGrid(i + 1)(j + 1) = grid(i)(j)
      }
    }

    newGrid
  }

  private val gridSurroundedWithBLOCK = surroundedWithOnes(grid)

  private def isBlock(node: Node, dir: (Int, Int)): Boolean = {
    gridSurroundedWithBLOCK(node.x + dir._1 + 1)(node.y + dir._2 + 1) == BLOCK
  }

  private def isBlock(pos: (Int, Int), dir: (Int, Int)): Boolean = {
    gridSurroundedWithBLOCK(pos._1 + dir._1 + 1)(pos._2 + dir._2 + 1) == BLOCK
  }

  private def isBlock(pos: (Int, Int)): Boolean = {
    gridSurroundedWithBLOCK(pos._1 + 1)(pos._2 + 1) == BLOCK
  }

  private def isBlock(x: Int, y: Int): Boolean = {
    gridSurroundedWithBLOCK(x + 1)(y + 1) == BLOCK
  }

  private def heuristic(begin: (Int, Int), end: (Int, Int)): Double = {
    val dx = (begin._1 - end._1).abs
    val dy = (begin._2 - end._2).abs
    dx.min(dy) * DiagonalMoveCost + (dx - dy).abs * StraightMoveCost
  }

  private def reconstructPath(node: Node): List[Node] = {
    @tailrec
    def loop(current: Node, path: List[Node]): List[Node] = {
      current.parent match { case None => current :: path
        case Some(parent) => loop(parent, current :: path)
      }
    }

    loop(node, Nil)
  }

  private def DiagonalMove(dir: (Int, Int)): Boolean = {
    if (dir._1 == 0 || dir._2 == 0) false else true
  }

  var iterCount = 0
  private def perIter(): Unit = {
//        printMap()
//        open.foreach(println)
//        println(s"closed: ${closed.size}, open: ${open.size}")
//        printMapNearby((240, 439), 30)
//        if (iterCount == 0) {
//          printMap()
//          iterCount = StdIn.readInt()
//        }
//        iterCount -= 1
  }

  @tailrec
  private final def jpsSearch(): List[Node] = {
    if (open.isEmpty) Nil
    else {
      perIter()
      val current = open.dequeue()
      if (current == end) reconstructPath(current)
      else if (closed.contains(current)) jpsSearch()
      else {
        def addJumpPoint(node: Node, dir: List[(Int, Int)]): Unit = {
          open.enqueue(node.copy(g = heuristic(current, node) + current.g, h = heuristic(node, end), parent = Some(current), dir = dir))
        }

        def jumpPoint(pos: (Int, Int), dir: (Int, Int)): Unit = {
          val dx = dir._1
          val dy = dir._2
          val x = pos._1
          val y = pos._2
          if (pos == end) {
            open.enqueue(Node(end._1, end._2, 0, 0, Some(current), Nil))
          } else {
            val b1 = !isBlock(x - dx, y + dy) && isBlock(x - dx, y)
            val b2 = !isBlock(x + dx, y - dy) && isBlock(x, y - dy)
            //            斜向穿墙
            //            if (b1 && b2) addJumpPoint(node, d1 :: d3 :: Nil)
            if (b1) {
              addJumpPoint(Node(pos), (-dx, dy) :: Nil)
            } else if (b2) {
              addJumpPoint(Node(pos), (dx, -dy) :: Nil)
            }
          }
        }

        def jumpPointsInLine(pos: (Int, Int), dir: (Int, Int)): Unit = {
          //逆时针45度
          def getUpDir(tuple: (Int, Int)): (Int, Int) = {
            val dx = tuple._1
            val dy = tuple._2
            (dx - dy, dy + dx)
          }

          //顺时针45度
          def getDownDir(tuple: (Int, Int)): (Int, Int) = {
            val dx = tuple._1
            val dy = tuple._2
            (dx + dy, dy - dx)
          }

          def path(node: (Int, Int), dir: (Int, Int)): Unit = {
            val dx = dir._1
            val dy = dir._2
            val upDir = getUpDir(dir)
            val downDir = getDownDir(dir)
            def getKthStep(pos: (Int, Int), k: Int): (Int, Int) = {
              (pos._1 + dir._1 * k, pos._2 + dir._2 * k)
            }

            def getCurNodes(node: (Int, Int), offset: Int): Array[(Int, Int)] = {
              val newNode = getKthStep(node, offset * jpsBitMap.BITSPERCELL)
              if (dx == 0 && dy > 0) {
                Array((newNode._1 - 1, newNode._2), newNode, (newNode._1 + 1, newNode._2))
              }else if (dx > 0 && dy == 0) {
                Array((newNode._1, newNode._2 + 1), newNode, (newNode._1, newNode._2 - 1))
              } else if (dx == 0 && dy < 0) {
                Array((newNode._1 + 1, newNode._2), newNode, (newNode._1 - 1, newNode._2))
              } else {
                Array((newNode._1, newNode._2 - 1), newNode, (newNode._1, newNode._2 + 1))
              }
            }

            def getMaskOffSet(node: (Int, Int)): Int = {
              if (dx == 0 && dy > 0){
                node._2 % jpsBitMap.BITSPERCELL
              } else if (dx == 0 && dy < 0) {
                31 - node._2 % jpsBitMap.BITSPERCELL
              } else if (dx > 0 && dy == 0) {
                node._1 % jpsBitMap.BITSPERCELL
              } else {
                31 - node._1 % jpsBitMap.BITSPERCELL
              }
            }

            //防止 -1 变为Long后仍是 -1
            def makeLong(low: Int, high: Int): Long = {
              (high.toLong << 32) | (low & 0xffffffffL)
            }

            @tailrec
            def loop(posU: Int, posD: Int, posWall: Int, pathMPre: Int, pathUPre: Int, pathDPre: Int, offset: Int): (Int, Int, Int) = {
              if (posU != 0 || posD != 0 || posWall != 0 && (if (offset == 0) posU > offset || posD > offset || posWall > offset else true)) {
                val newUPos = if (posU == 0) 1 << 30 else posU + offset * jpsBitMap.BITSPERCELL
                val newDPos = if (posD == 0) 1 << 30 else posD + offset * jpsBitMap.BITSPERCELL
                val newWallPos = if (posWall == 0) (offset + 2) * jpsBitMap.BITSPERCELL else posWall + offset * jpsBitMap.BITSPERCELL
                (newUPos, newDPos, newWallPos)
              } else {
                val nodes = getCurNodes(node, offset + 2)
                val pathMNext: Int = jpsBitMap.getCells(nodes(1), dir)
                val pathUNext: Int = jpsBitMap.getCells(nodes(0), dir)
                val pathDNext: Int = jpsBitMap.getCells(nodes(2), dir)
                val pathM: Long = makeLong(pathMPre, pathMNext)
                val pathU: Long = makeLong(pathUPre, pathUNext)
                val pathD: Long = makeLong(pathDPre, pathDNext)
                val posU = nativeCallObj.ffsll((pathU >> 1) & ~pathU)
                val posD = nativeCallObj.ffsll((pathD >> 1) & ~pathD)
                val posWall = nativeCallObj.ffsll(~pathM)
                loop(posU, posD, posWall, pathMNext, pathUNext, pathDNext, offset + 1)
              }
            }

            val nodes = getCurNodes(node, 0)
            val maskOffset = getMaskOffSet(node)
            val pathMPre: Int = jpsBitMap.getMaskedCells(nodes(1), dir, maskOffset)
            val pathUPre: Int = jpsBitMap.getMaskedCells(nodes(0), dir, maskOffset)
            val pathDPre: Int = jpsBitMap.getMaskedCells(nodes(2), dir, maskOffset)
            val pathMNext: Int = jpsBitMap.getCells(getKthStep(nodes(1), jpsBitMap.BITSPERCELL), dir)
            val pathUNext: Int = jpsBitMap.getCells(getKthStep(nodes(0), jpsBitMap.BITSPERCELL), dir)
            val pathDNext: Int = jpsBitMap.getCells(getKthStep(nodes(2), jpsBitMap.BITSPERCELL), dir)
            val pathM: Long = makeLong(pathMPre, pathMNext)
            val pathU: Long = makeLong(pathUPre, pathUNext)
            val pathD: Long = makeLong(pathDPre, pathDNext)
            val firstPosU = nativeCallObj.ffsll((pathU >> 1) & ~pathU)
            val firstPosD = nativeCallObj.ffsll((pathD >> 1) & ~pathD)
            val firstPosWall = nativeCallObj.ffsll(~pathM)
            val (posU, posD, wallPos) = loop(firstPosU, firstPosD, firstPosWall, pathMNext, pathUNext, pathDNext, 0)


            if (node._1 == end._1 && (node._2 - end._2).abs < wallPos) {
              open.enqueue(Node(end._1, end._2, 0, 0, Some(current), Nil))
            } else if (node._2 == end._2 && (node._1 - end._1).abs < wallPos) {
              open.enqueue(Node(end._1, end._2, 0, 0, Some(current), Nil))
            }else if (posU == posD && posU < wallPos - 1) {
              addJumpPoint(Node(getKthStep(node, posU - 1 - maskOffset)), dir :: upDir :: downDir :: Nil)
            } else if (posU < wallPos && posU < posD) {
              addJumpPoint(Node(getKthStep(node, posU - 1 - maskOffset)), dir :: upDir :: Nil)
            } else if (posD < wallPos && posD < posU) {
              addJumpPoint(Node(getKthStep(node, posD - 1 - maskOffset)), dir :: downDir :: Nil)
            }
          }

          path(pos, dir)
        }

        closed += current
        val directions = current.dir
        directions.foreach { dir =>
          val curNode = (current.x + dir._1, current.y + dir._2)
          if (DiagonalMove(dir)) {
            @tailrec
            def f(pos: (Int, Int)): Unit = {
              if (!isBlock(pos) && !closed.contains(pos) && !open.exists(n => n.x == pos._1 && n.y == pos._2)) {
                jumpPoint(pos, dir)
                jumpPointsInLine((pos._1 + dir._1, pos._2), (dir._1, 0))
                jumpPointsInLine((pos._1, pos._2 + dir._2), (0, dir._2))
                f((pos._1 + dir._1, pos._2 + dir._2))
              }
            }

            f(curNode)
          } else {
            jumpPointsInLine(curNode, dir)
          }
        }
        jpsSearch()
      }
    }
  }

  def jps(): List[Node] = {
    closed.clear()
    open.clear()
    open.enqueue(new Node(start._1, start._2, 0, 0, None, (0, 1) :: (0, -1) :: (1, 0) :: (-1, 0) :: (1, 1) :: (1, -1) :: (-1, 1) :: (-1, -1) :: Nil))
    jpsSearch()
  }

  private def writeMap(): Unit = {
    val fos = new FileOutputStream(s"jpsmap$start$end.txt")
    println(s"closed: ${closed.size}, open: ${open.size}")
    println(s"grid.length: ${grid.length}, grid(0).length: ${grid(0).length}")
    grid.indices.foreach { i =>
      grid(i).indices.foreach { j =>
        if (closed.exists(n => n._1 == i && n._2 == j)) fos.write("C ".getBytes)
        else if (open.exists(n => n.x == i && n.y == j)) fos.write("P ".getBytes)
        else fos.write(s"${grid(i)(j)} ".getBytes)
      }
      fos.write("\n".getBytes)
    }
  }

  private def printMapNearby(pos: (Int, Int), sideLength: Int): Unit = {
    val (x, y) = pos
    val (rows, cols) = (grid.length, grid(0).length)
    val (startX, startY) = (x - sideLength, y - sideLength)
    val (endX, endY) = (x + sideLength, y + sideLength)
    for (i <- startX to endX) {
      for (j <- startY to endY) {
        if (i == x && j == y) print("T ")
        else if (closed.exists(n => n._1 == i && n._2 == j)) print("C ")
        else if (open.exists(n => n.x == i && n.y == j)) print("P ")
        else if (i < 0 || i >= rows || j < 0 || j >= cols) print("B ")
        else print(s"${grid(i)(j)} ")
      }
      println()
    }
    println()
  }

  private def printMap(): Unit = {
    grid.indices.foreach { i =>
      grid(i).indices.foreach { j =>
        if (closed.exists(n => n._1 == i && n._2 == j)) print("C ")
        else if (open.exists(n => n.x == i && n.y == j)) print("P ")
        else print(s"${grid(i)(j)} ")
      }
      println()
    }
    println()
  }
}

object Jps {
  implicit def nodeToTuple(node: Node): (Int, Int) = (node.x, node.y)

  object Node {
    def apply(x: Int, y: Int): Node = {
      new Node(x, y, 0, 0, None, Nil)
    }

    def apply(para: (Int, Int)): Node = {
      new Node(para._1, para._2, 0, 0, None, Nil)
    }
  }

  case class Node(x: Int, y: Int, g: Double, h: Double, parent: Option[Node], dir: List[(Int, Int)]) extends Ordered[Node] {
    def f: Double = g + h

    override def equals(obj: Any): Boolean = obj match {
      case (x, y) => this.x == x && this.y == y
      case that: Node => this.x == that.x && this.y == that.y
      case _ => false
    }

    override def hashCode(): Int = ((41 * x) + y) * 41

    override def compare(that: Node): Int = {
      val n = if (this.f == that.f) that.h - this.h else that.f - this.f
      if (n > 0) 1 else if (n < 0) -1 else 0
    }

    override def toString: String = s"Node($x, $y, $g, $h, $f, $dir, ${
      parent match {
        case Some(p) => p
        case None => "None"
      }
    })"
  }

  def main(args: Array[String]): Unit = {
    testGroup1()
  }

  private def testGroup1(): Unit = {
    val grid: Array[Array[Byte]] = Array(
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    )
//    val path = new Jps((2, 2), (8, 7), grid).jps()
//    path.foreach(node => println(s"${node.x}, ${node.y}"))
  }

  def testGroup2(): Unit = {
    val grid: Array[Array[Byte]] = Array(
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    )
//    val path = new Jps((1, 1), (5, 8), grid).jps()
//    path.foreach(node => println(s"${node.x}, ${node.y}"))
  }
}
