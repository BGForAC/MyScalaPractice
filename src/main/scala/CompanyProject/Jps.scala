package CompanyProject

import CompanyProject.Jps.Node

import java.io.FileOutputStream
import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

class Jps(start: (Int, Int), end: (Int, Int), grid: Array[Array[Byte]]) {
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
  private final val BLOCK = 0
  private final val EMPTY = 1
  private final val StraightMoveCost = 10
  private final val DiagonalMoveCost = 14

  private val closed = mutable.HashSet[(Int, Int)]()
  private val open = mutable.PriorityQueue[Node]()

  private def isBlock(node: Node, direction: (Int, Int)): Boolean = {
    if (outOfRange((node.x + direction._1, node.y + direction._2))) true
    else grid(node.x + direction._1)(node.y + direction._2) == BLOCK
  }

  private def heuristic(begin: (Int, Int), end: (Int, Int)): Double = {
    val (dx, dy) = ((begin._1 - end._1).abs, (begin._2 - end._2).abs)
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

  private def road(pos: (Int, Int), dir: (Int, Int)): List[(Int, Int)] = {
    val (dx, dy) = dir

    @tailrec
    def f(pos: (Int, Int), list: List[(Int, Int)]): List[(Int, Int)] = {
      if (outOfRange(pos) || grid(pos._1)(pos._2) == BLOCK) list
      else f((pos._1 + dx, pos._2 + dy), pos :: list)
    }

    f((pos._1 + dx, pos._2 + dy), Nil)
  }

  private def outOfRange(pos: (Int, Int)) = {
    pos._1 < 0 || pos._1 >= grid.length || pos._2 < 0 || pos._2 >= grid(0).length
  }

  private def isDiagonalMove(dir: (Int, Int)): Boolean = {
    if (dir._1 == 0 || dir._2 == 0) false else true
  }

  @tailrec
  private final def jpsSearch(): List[Node] = {
    if (open.isEmpty) Nil
    else {
      //      open.foreach(println)
      //      println()
      //      closed.foreach(println)
      //      println()
      //      println()
      val current = open.dequeue()
      if (current == end) reconstructPath(current)
      else if (closed.contains(current)) jpsSearch()
      else {
        def addJumpPoint(node: Node, dir: List[(Int, Int)]): Unit = {
          open.enqueue(node.copy(g = heuristic(current, node) + current.g, h = heuristic(node, end), parent = Some(current), dir = dir))
        }

        def jumpPoints(node: Node, dir: (Int, Int)): Unit = {
          def f(node: Node, d1: (Int, Int), d2: (Int, Int), d3: (Int, Int), d4: (Int, Int)): Unit = {
            val b1 = !isBlock(node, d1) && isBlock(node, d2)
            val b2 = !isBlock(node, d3) && isBlock(node, d4)
            if (b1 && b2) addJumpPoint(node, d1 :: d3 :: Nil)
            else if (b1) addJumpPoint(node, d1 :: Nil)
            else if (b2) addJumpPoint(node, d3 :: Nil)
          }

          val (dx, dy) = dir
          if (node == end) {
            open.enqueue(node.copy(g = 0, h = 0, parent = Some(current), dir = Nil))
          } else {
            f(node, (-dx, dy), (-dx, 0), (dx, -dy), (0, -dy))
          }
        }

        def jumpPointInLine(node: Node, dir: (Int, Int)): Unit = {
          def isBitSet(bigInt: BigInt, bitPosition: Int): Boolean = {
            (bigInt & (BigInt(1) << bitPosition)) != 0
          }

          //逆时针45度
          def getUdx(tuple: (Int, Int)): (Int, Int) = {
            tuple match {
              case (0, 1) => (-1, 1)
              case (1, 0) => (1, 1)
              case (0, -1) => (1, -1)
              case (-1, 0) => (-1, -1)
            }
          }

          //顺时针45度
          def getDdx(tuple: (Int, Int)): (Int, Int) = {
            tuple match {
              case (0, 1) => (1, 1)
              case (1, 0) => (1, -1)
              case (0, -1) => (-1, -1)
              case (-1, 0) => (-1, 1)
            }
          }

          /**
           * 获得3 * n的矩阵，n为包括node点到尽头（墙或边界）的长度
           * @param node
           * @param dir
           * @param idx
           * @return
           */
          def getPath(node: (Int, Int), dir: (Int, Int), idx: Int): Array[BigInt] = {
            val (dx, dy) = dir
            var (x, y) = node
            var (xu, yu) = (0, 0)
            var (xd, yd) = (0, 0)
            if (dx == 0) {
              xu = x - dy
              yu = y
              xd = x + dy
              yd = y
            } else {
              xu = x
              yu = y + dx
              xd = x
              yd = y + dx
            }
            val array = Array.fill[BigInt](3)(BigInt(0))
            val newGrid: Array[Array[Int]] = getNewGrid()

            def buildPath(idx: Int): Array[BigInt] = {
              if (outOfRange(x, y) || grid(x)(y) == BLOCK) array
              else {
                if (newGrid(xu + 1)(yu + 1) == 1) array(0).setBit(idx)
                if (newGrid(x + 1)(y + 1) == 1) array(1).setBit(idx)
                if (newGrid(xd + 1)(yd + 1) == 1) array(2).setBit(idx)
                x += dx
                xu += dx
                xd += dx
                y += dy
                yu += dy
                yd += dy
                buildPath(idx + 1)
              }
            }
            buildPath(idx)
          }

          /**
           * x  ? ? ? ? ? ?
           * w  0 0 0 0 0 0
           * x  ? ? ? ? ? ?
           */
          val ud = getUdx(dir)
          val dd = getDdx(dir)
          val route: Array[BigInt] = getPath(node, dir, 1)
          val uPos = ~route(0) >> 1 & route(0)
          val dPos = ~route(2) >> 1 & route(2)
          for (i <- 0 until route(1).bitLength) {
            val b1 = isBitSet(uPos, i)
            val b2 = isBitSet(dPos, i)
            if (b1 && b2) addJumpPoint(node, ud :: dd :: Nil)
            else if (b1) addJumpPoint(node, ud :: Nil)
            else if (b2) addJumpPoint(node, dd :: Nil)
            else {
              //不需要做什么
            }
          }

        }

        closed += current
        val directions = current.dir
        directions.foreach { dir =>
          val curNode = Node(current.x + dir._1, current.y + dir._2)
          if (isDiagonalMove(dir)) {
            @tailrec
            def f(node: Node): Unit = {
              if (!outOfRange(node)) {
                jumpPoints(node, dir)
                jumpPointInLine(node, (dir._1, 0))
                jumpPointInLine(node, (0, dir._2))
                f(Node((node.x + dir._1, node.y + dir._2)))
              }
            }

            f(curNode)
          } else {
            jumpPointInLine(curNode, dir)
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

    override def toString: String = s"Node($x, $y, $g, $h, $f, ${
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
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    )
    val path = new Jps((2, 2), (10, 11), grid).jps()
    path.foreach(node => println(s"${node.x}, ${node.y}"))
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
    val path = new Jps((1, 1), (5, 8), grid).jps()
    path.foreach(node => println(s"${node.x}, ${node.y}"))
  }
}
