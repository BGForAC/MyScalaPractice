package CompanyProject

import CompanyProject.Jps.Node

import java.io.FileOutputStream
import scala.annotation.tailrec
import scala.collection.mutable

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
  private val open = mutable.TreeSet[Node]()
  open.add(Node(start))

  private def isBlock(node: Node, direction: (Int, Int)): Boolean = {
    if (outOfRange((node.x + direction._1, node.y + direction._2))) true
    else grid(node.x + direction._1)(node.y + direction._2) == BLOCK
  }

  private def isJumpPoint(node: Node, dir: (Int, Int)): Boolean = {
    def f(para: (Node, Array[(Int, Int)])): Boolean = {
      val (node, Array(d1, d2, d3, d4)) = para
      val (b1, b2) = (!isBlock(node, d1) && isBlock(node, d2), !isBlock(node, d3) && isBlock(node, d4))
      if (b1 && b2) open.add(node.copy(g = heuristic(current, node) + current.g, h = heuristic(node, end), parent = current, dir = d1 :: d3 :: Nil))
      else if (b1) open.add(node.copy(g = heuristic(current, node) + current.g, h = heuristic(node, end), parent = current, dir = d1 :: Nil))
      else if (b2) open.add(node.copy(g = heuristic(current, node) + current.g, h = heuristic(node, end), parent = current, dir = d3 :: Nil))
      b1 || b2
    }

    val (dx, dy) = dir
    if (node == end) true
    else if (isDiagonalMove(dir)) {
      f((node, Array((-dx, dy), (-dx, 0), (dx, -dy), (0, -dy))))
    } else {
      if (dy == 0) {
        f((node, Array((dx, 1), (0, 1), (dx, -1), (0, -1))))
      } else {
        f((node, Array((1, dy), (1, 0), (-1, dy), (-1, 0))))
      }
    }
  }

  private def haveJumpPointInLine(node: Node, dir: (Int, Int)): Boolean = {
    val direction = Array((dir._1, 0), (0, dir._2))
    var find = false
    direction.foreach { dir =>
      val isFind = road(node, dir).exists(pos => isJumpPoint(Node(pos), dir))
      find = find || isFind
    }
    find
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

  private def road(pos: Any, dir: (Int, Int)): List[(Int, Int)] = {
    val (dx, dy) = dir

    @tailrec
    def f(pos: (Int, Int), list: List[(Int, Int)]): List[(Int, Int)] = {
      val (x, y) = pos
      if (outOfRange(pos) || grid(x)(y) == BLOCK) list
      else f((x + dx, y + dy), pos :: list)
    }

    pos match {
      case (x: Int, y: Int) => f((x + dx, y + dy), Nil)
      case Node(x, y, _, _, _, _) => f((x + dx, y + dy), Nil)
      case _ => Nil
    }
  }

  private def outOfRange(pos: (Int, Int)) = {
    val (x, y) = pos
    x < 0 || x >= grid.length || y < 0 || y >= grid(0).length
  }

  private def isDiagonalMove(dir: (Int, Int)): Boolean = {
    if (dir._1 == 0 || dir._2 == 0) false else true
  }

  @tailrec
  private final def jpsSearch(): List[Node] = {
    if (open.isEmpty) Nil
    else {
      val current = open.last
      open -= current
      if (current == end) reconstructPath(current)
      else if (closed.contains(current)) jpsSearch()
      else {
        closed += current
        val directions = current.dir
        directions.foreach {
          dir =>
            road(current, dir).foreach {
              pos => {
                val node = Node(pos)

                if (!closed.contains(Node(pos)) && (isJumpPoint(node, dir) || (isDiagonalMove(dir) && haveJumpPointInLine(node, dir)))) {
                  open.add(node.copy(g = current.g + heuristic(current, node), h = heuristic(current, end), parent = Some(current)))
                }
              }
            }
        }
        jpsSearch()
      }
    }
  }

  def jps(): List[Node] = {
    val iterCount = 1
    var temp: List[Node] = Nil
    for (_ <- 0 until iterCount) temp = jpsSearch()
    temp
  }

  private def writeMap(): Unit = {
    val fos = new FileOutputStream(s"jspmap${start}${end}.txt")
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

  def testGroup1() = {
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
