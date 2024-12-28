package PathFindingAlgorithm

import PathFindingAlgorithm.Jps.Node

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

class Jps(start: (Int, Int), end: (Int, Int), grid: Array[Array[Byte]], jpsBitMap: JpsBitMap) {
  private final val nativeCallObj = new NativeCall()

  private final val OBSTACLE: Byte = 0
  private final val EMPTY: Byte = 1
  private final val StraightMoveCost = 10
  private final val DiagonalMoveCost = 14

  private val closed = mutable.HashSet[Int]()
  private val open = mutable.PriorityQueue[Node]()

  private def surroundedWithObstacle(grid: Array[Array[Byte]]): Array[Array[Byte]] = {
    val rows = grid.length
    val cols = grid(0).length
    val newGrid: Array[Array[Byte]] = Array.fill(rows + 2, cols + 2)(OBSTACLE)

    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        newGrid(i + 1)(j + 1) = grid(i)(j)
      }
    }

    newGrid
  }

  private val gridSurroundedByObstacle = surroundedWithObstacle(grid)

  private def isBlock(node: Node, dir: (Int, Int)): Boolean = {
    gridSurroundedByObstacle(node.x + dir._1 + 1)(node.y + dir._2 + 1) == OBSTACLE
  }

  private def isBlock(pos: (Int, Int), dir: (Int, Int)): Boolean = {
    gridSurroundedByObstacle(pos._1 + dir._1 + 1)(pos._2 + dir._2 + 1) == OBSTACLE
  }

  private def isBlock(pos: (Int, Int)): Boolean = {
    gridSurroundedByObstacle(pos._1 + 1)(pos._2 + 1) == OBSTACLE
  }

  private def isBlock(x: Int, y: Int): Boolean = {
    gridSurroundedByObstacle(x + 1)(y + 1) == OBSTACLE
  }

  private def heuristic(begin: (Int, Int), end: (Int, Int)): Double = {
    val dx = (begin._1 - end._1).abs
    val dy = (begin._2 - end._2).abs
    dx.min(dy) * DiagonalMoveCost + (dx - dy).abs * StraightMoveCost
  }

  private def reconstructPath(node: Node): List[Node] = {
    @tailrec
    def loop(current: Node, path: List[Node]): List[Node] = {
      current.parent match {
        case None => current :: path
        case Some(parent) => loop(parent, current :: path)
      }
    }

    loop(node, Nil)
  }

  private def DiagonalMove(dir: (Int, Int)): Boolean = {
    if (dir._1 == 0 || dir._2 == 0) false else true
  }

  @tailrec
  private final def jpsSearch(): List[Node] = {
    if (open.isEmpty) Nil
    else {
      val current = open.dequeue()
      if (current == end) reconstructPath(current)
      else if (closed.contains((current.x << 16) | current.y)) jpsSearch()
      else {
        def addJumpPoint(node: Node, dir: List[(Int, Int)]): Unit = {
          open.enqueue(node.copy(g = heuristic(current, node) + current.g, h = heuristic(node, end), parent = Some(current), dir = dir))
        }

        def jumpPoint(x: Int, y: Int, dx: Int, dy: Int): Unit = {
          if ((x, y) == end) {
            open.enqueue(Node(end._1, end._2, 0, 0, Some(current), Nil))
          } else {
            val b1 = !isBlock(x - dx, y + dy) && isBlock(x - dx, y)
            val b2 = !isBlock(x + dx, y - dy) && isBlock(x, y - dy)
            if (b1) {
              addJumpPoint(Node(x, y), (-dx, dy) :: Nil)
            } else if (b2) {
              addJumpPoint(Node(x, y), (dx, -dy) :: Nil)
            }
          }
        }

        def jumpPointsInLine(x: Int, y: Int, dx: Int, dy: Int): Unit = {
          def path(x: Int, y: Int, dx: Int, dy: Int): Unit = {
            val dir = (dx, dy)
            val upDir = (dx - dy, dy + dx)
            val downDir = (dx + dy, dy - dx)

            def getKthStep(pos: (Int, Int), k: Int): (Int, Int) = {
              (pos._1 + dx * k, pos._2 + dy * k)
            }

            //防止 -1 变为Long后仍是 -1
            def makeLong(low: Int, high: Int): Long = {
              (high.toLong << 32) | (low & 0xffffffffL)
            }

            @tailrec
            def loop(posU: Int, posD: Int, posWall: Int, pathMPre: Int, pathUPre: Int, pathDPre: Int, offset: Int): (Int, Int, Int) = {
              if (posU != 0 || posD != 0 || posWall != 0 && (if (offset == 0) posU > offset || posD > offset || posWall > offset else true)) {
                val newUPos = if (posU == 0) 1 << 30 else posU + (offset << jpsBitMap.BITOFFSET)
                val newDPos = if (posD == 0) 1 << 30 else posD + (offset << jpsBitMap.BITOFFSET)
                val newWallPos = if (posWall == 0) (offset + 2) << jpsBitMap.BITOFFSET else posWall + (offset << jpsBitMap.BITOFFSET)
                (newUPos, newDPos, newWallPos)
              } else {
                val nX = x + (dx * ((offset + 2) << jpsBitMap.BITOFFSET))
                val nY = y + (dy * ((offset + 2) << jpsBitMap.BITOFFSET))
                val pathMNext: Int = jpsBitMap.getCells(nX, nY, dx, dy)
                val pathUNext: Int = jpsBitMap.getCells(nX - dy, nY + dx, dx, dy)
                val pathDNext: Int = jpsBitMap.getCells(nX + dy, nY - dx, dx, dy)
                val pathM: Long = makeLong(pathMPre, pathMNext)
                val pathU: Long = makeLong(pathUPre, pathUNext)
                val pathD: Long = makeLong(pathDPre, pathDNext)
                val posU = nativeCallObj.ffsll((pathU >> 1) & ~pathU)
                val posD = nativeCallObj.ffsll((pathD >> 1) & ~pathD)
                val posWall = nativeCallObj.ffsll(~pathM)
                loop(posU, posD, posWall, pathMNext, pathUNext, pathDNext, offset + 1)
              }
            }

            val maskOffset = dy * (y & jpsBitMap.BITMASK) + dx * (x & jpsBitMap.BITMASK) + (((dx | dy) - 1) >>> 27)
            val pathMPre: Int = jpsBitMap.getMaskedCells(x, y, dx, dy, maskOffset)
            val pathUPre: Int = jpsBitMap.getMaskedCells(x - dy, y + dx, dx, dy, maskOffset)
            val pathDPre: Int = jpsBitMap.getMaskedCells(x + dy, y - dx, dx, dy, maskOffset)
            val nX = x + (dx << jpsBitMap.BITOFFSET)
            val nY = y + (dy << jpsBitMap.BITOFFSET)
            val pathMNext: Int = jpsBitMap.getCells(nX, nY, dx, dy)
            val pathUNext: Int = jpsBitMap.getCells(nX - dy, nY + dx, dx, dy)
            val pathDNext: Int = jpsBitMap.getCells(nX + dy, nY - dx, dx, dy)
            val pathM: Long = makeLong(pathMPre, pathMNext)
            val pathU: Long = makeLong(pathUPre, pathUNext)
            val pathD: Long = makeLong(pathDPre, pathDNext)
            val firstPosU = nativeCallObj.ffsll((pathU >> 1) & ~pathU)
            val firstPosD = nativeCallObj.ffsll((pathD >> 1) & ~pathD)
            val firstPosWall = nativeCallObj.ffsll(~pathM)
            val (newUPos, newDPos, newWallPos) = loop(firstPosU, firstPosD, firstPosWall, pathMNext, pathUNext, pathDNext, 0)

            if (x == end._1 && (y - end._2).abs < (newWallPos - maskOffset) && (dy > 0 && y < end._2 || dy < 0 && y > end._2)) {
              open.enqueue(Node(end._1, end._2, 0, 0, Some(current), Nil))
            } else if (y == end._2 && (x - end._1).abs < (newWallPos - maskOffset) && (dx > 0 && x < end._1 || dx < 0 && x > end._1)) {
              open.enqueue(Node(end._1, end._2, 0, 0, Some(current), Nil))
            } else if (newUPos == newDPos && newUPos < newWallPos - 1) {
              addJumpPoint(Node(getKthStep((x, y), newUPos - 1 - maskOffset)), dir :: upDir :: downDir :: Nil)
            } else if (newUPos < newWallPos && newUPos < newDPos) {
              addJumpPoint(Node(getKthStep((x, y), newUPos - 1 - maskOffset)), dir :: upDir :: Nil)
            } else if (newDPos < newWallPos && newDPos < newUPos) {
              addJumpPoint(Node(getKthStep((x, y), newDPos - 1 - maskOffset)), dir :: downDir :: Nil)
            }
          }

          path(x, y, dx, dy)
        }

        closed += (current.x << 16) | current.y
        val directions = current.dir
        directions.foreach { dir =>
          val (dx, dy) = dir
          if (DiagonalMove(dir)) {
            @tailrec
            def f(x: Int, y: Int): Unit = {
              if (!isBlock(x, y) && !closed.contains((x << 16) | y)) {
                jumpPoint(x, y, dx, dy)
                jumpPointsInLine(x + dx, y, dx, 0)
                jumpPointsInLine(x, y + dy, 0, dy)
                f(x + dx, y + dy)
              }
            }

            f(current.x + dx, current.y + dy)
          } else {
            jumpPointsInLine(current.x + dx, current.y + dy, dx, dy)
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
}
