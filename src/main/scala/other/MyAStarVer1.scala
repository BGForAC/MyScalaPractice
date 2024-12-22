package other

import scala.annotation.tailrec
import scala.collection.mutable

object MyAStarVer1 {
  private val StraightMoveCost: Double = 10.0
  private val DiagonalMoveCost: Double = 14.1

  case class Node(x: Int, y: Int, g: Double, h: Double, parent: Option[Node]) extends Ordered[Node] {
    def f: Double = g + h

    override def compare(that: Node): Int = (if (this.f == that.f) that.h - this.h else that.f - this.f).toInt

    override def equals(that: Any): Boolean = { that match {
      case (t1, t2) => t1 == this.x && t2 == this.y
      case obj: Node => equals(obj.x, obj.y)
      case _ => false
    }}

    override def hashCode(): Int = ((this.x * 41) + y) * 41

    override def toString: String = s"Node($x, $y, $g, $h, $f, ${parent match {
      case Some(p) => p
      case None => "None"
    }})"
  }

  def reconstructPath(node: Node): List[Node] = {
    @tailrec
    def loop(current: Node, path: List[Node]): List[Node] = {
      current.parent match {
        case Some(parent) => loop(parent, current :: path)
        case None => current :: path
      }
    }

    loop(node, Nil)
  }

  private def moveCost(tuple2: (Int, Int)): Double = {
    if (tuple2._1.abs == tuple2._2.abs) DiagonalMoveCost else StraightMoveCost
  }

  def neighbors(node: Node, grid: Array[Array[Int]]): List[Node] = {
    assert(grid.length > 0, "地图大小不规范")
    val (mx, my) = (grid.length, grid(0).length)
    val directions: List[(Int, Int)] = List((1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))
    directions.flatMap{case (dx, dy) =>
      val (x, y) = (node.x + dx, node.y + dy)
      if (x < 0 || mx <= x || y < 0 || my <= y || grid(x)(y) == 1) None
      else Some(Node(x, y, node.g + moveCost((dx, dy)), 0, Some(node)))
    }
  }

  def heuristic(begin: Node, end: Node): Double = {
    val (dx, dy) = ((begin.x - end.x).abs, (begin.y -end.y).abs)
    dx.min(dy) * DiagonalMoveCost + (dx - dy).abs * StraightMoveCost
  }

  private def AStar(start: Node, end: Node, grid: Array[Array[Int]]): List[Node] = {
    val open = mutable.PriorityQueue[Node]()
    val closed = mutable.HashSet[Node]()
    open.enqueue(start)

    def printMap(): Unit = {
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

    @tailrec
    def f(): List[Node] = {
      if (open.isEmpty) Nil
      else {
        printMap()
        val current = open.dequeue()
        if (current == end) reconstructPath(current)
        else {
          closed += current
          neighbors(current, grid)
            .filter(neighbor => !closed.contains(neighbor) &&
              !open.exists(node => node.g < neighbor.g && node == neighbor))
            .map(neighbor => neighbor.copy(h = heuristic(current, end)))
            .foreach(open.enqueue(_))
          f()
        }
      }
    }

    f()
  }

  def main(args: Array[String]): Unit = {
    val grid = Array(
      Array(0, 1, 0, 0, 0),
      Array(0, 1, 0, 1, 0),
      Array(0, 0, 0, 1, 0),
      Array(0, 1, 0, 0, 0),
      Array(0, 0, 0, 1, 0)
    )
    val start = Node(0, 0, 0, 0, None)
    val goal = Node(4, 4, 0, 0, None)
    val path = AStar(start, goal, grid)
    path.foreach(node => println(s"(${node.x}, ${node.y})"))
  }
}