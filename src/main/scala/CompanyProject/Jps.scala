package CompanyProject

import CompanyProject.Jps.Node

import java.io.FileOutputStream
import scala.annotation.tailrec
import scala.collection.immutable.List
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

  private val jumpPointSet = mutable.HashSet[Node]()
  private val directionMap = mutable.HashMap[Node,mutable.ListBuffer[(Int, Int)]]()
  private val closed = mutable.HashSet[Node]()
  private val open = mutable.TreeSet[Node]()
  jumpPointSet += new Node(start)
  jumpPointSet += new Node(end)
  directionMap(new Node(start)) = mutable.ListBuffer((1, 0), (0, 1), (1, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))
  open.add(new Node(start))

  private def isBlock(node: Node, direction: (Int, Int)): Boolean = {
    if (outOfRange((node.x + direction._1, node.y + direction._2))) true
    else grid(node.x + direction._1)(node.y + direction._2) == BLOCK
  }

  private def isJumpPoint(node: Node, dir: (Int, Int)): Boolean = {
    val (dx, dy) = dir
    val dirList = mutable.ListBuffer[(Int, Int)]()
    var find = false
    if (jumpPointSet.contains(node)) find = true
    else if (isDiagonalMove(dir)) {
      if (!isBlock(node, (-dx, dy)) && isBlock(node, (-dx, 0))) {
        dirList += ((-dx, dy))
        find = true
      }
      if (!isBlock(node, (dx, -dy)) && isBlock(node, (0, -dy))) {
        dirList += ((dx, -dy))
        find =true
      }
    } else {
      if (dy == 0) {
        if (!isBlock(node, (dx, 1)) && isBlock(node, (0, 1))) {
          dirList += ((dx, 1))
          find = true
        }
        if (!isBlock(node, (dx, -1)) && isBlock(node, (0, -1))) {
          dirList += ((dx, -1))
          find = true
        }
      } else {
        if (!isBlock(node, (1, dy)) && isBlock(node, (1, 0))) {
          dirList += ((1, dy))
          find = true
        }
        if (!isBlock(node, (-1, dy)) && isBlock(node, (-1, 0))) {
          dirList += ((-1, dy))
          find = true
        }
      }
    }
    if (find) {
      if (!directionMap.contains(node)) directionMap(node) = mutable.ListBuffer()
      directionMap(node) ++= dirList
      jumpPointSet += node
    }
    find
  }

  private def haveJumpPointInLine(node: Node, dir: (Int, Int)): Boolean = {
    val direction = Array((dir._1, 0), (0, dir._2))
    var find = false
    direction.foreach(
      dir => {
        val isFind = road(node, dir).exists(pos => isJumpPoint(new Node(pos._1, pos._2), dir))
        if (isFind) directionMap(node) = directionMap.getOrElse(node, mutable.ListBuffer()) += dir
        find = find || isFind
      }
    )
    find
  }

  private def heuristic(begin: Node, end: Node): Double = {
    val (dx, dy) = ((begin.x - end.x).abs, (begin.y - end.y).abs)
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

  /**
   * 不包含pos的路径列表
   *
   * @param pos "位置"
   * @param dir “移动方向”
   * @return
   */
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
      case Node(x, y, _, _, _) => f((x + dx, y + dy), Nil)
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
        val directions = directionMap(current)
        directions.foreach {
          dir => road(current, dir).foreach {
            pos => {
              val node = new Node(pos._1, pos._2)

              if (!closed.contains(new Node(pos)) && (isJumpPoint(node, dir) || (isDiagonalMove(dir) && haveJumpPointInLine(node, dir)))) {
                open.add(node.copy(g = current.g + heuristic(current, node), h = heuristic(current, new Node(end._1, end._2)), parent = Some(current)))
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
    for ( _ <- 0 until iterCount) temp = jpsSearch()
    temp
  }

  private def writeMap(): Unit = {
    val fos = new FileOutputStream(s"jspmap${start}${end}.txt")
    println(s"closed: ${closed.size}, open: ${open.size}")
    println(s"grid.length: ${grid.length}, grid(0).length: ${grid(0).length}")
    grid.indices.foreach { i =>
      grid(i).indices.foreach { j =>
        if (closed.exists(n => n.x == i && n.y == j)) fos.write("C ".getBytes)
        else if (open.exists(n => n.x == i && n.y == j)) fos.write("P ".getBytes)
        else fos.write(s"${grid(i)(j)} ".getBytes)
      }
      fos.write("\n".getBytes)
    }
  }

  private def printMap(): Unit = {
    grid.indices.foreach { i =>
      grid(i).indices.foreach { j =>
        if (closed.exists(n => n.x == i && n.y == j)) print("C ")
        else if (open.exists(n => n.x == i && n.y == j)) print("P ")
        else print(s"${grid(i)(j)} ")
      }
      println()
    }
    println()
  }
}

object Jps {
  case class Node(x: Int, y: Int, g: Double, h: Double, parent: Option[Node]) extends Ordered[Node] {
    def f: Double = g + h

    def this(x: Int, y: Int) = this(x, y, 0, 0, None)

    def this(x: Int, y: Int, g: Double) = this(x, y, g, 0, None)

    def this(para: (Int, Int)) = this(para._1, para._2, 0, 0, None)

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
