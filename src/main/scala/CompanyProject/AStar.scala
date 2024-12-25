package CompanyProject

import java.io.FileOutputStream
import scala.annotation.tailrec
import scala.collection.mutable

class AStar(start: (Int, Int), end: (Int, Int), grid: Array[Array[Byte]]) {
  private final val StraightMoveCost = 10
  private final val DiagonalMoveCost = 14

  private val open = mutable.PriorityQueue[Node]()
  open.enqueue(Node(start._1, start._2, 0, 0, None))
  private val closed = mutable.HashSet[Node]()

  case class Node(x: Int, y: Int, g: Double, h: Double, parent: Option[Node]) extends Ordered[Node] {
    def f: Double = g + h

    override def compare(that: Node): Int = {
      val n = if (this.f == that.f) that.h - this.h else that.f - this.f
      if (n > 0) 1 else if (n < 0) -1 else 0
    }

    override def equals(that: Any): Boolean = {
      that match {
        case (t1, t2) => t1 == this.x && t2 == this.y
        case obj: Node => obj.x == this.x && obj.y == this.y
        case _ => false
      }
    }

    override def hashCode(): Int = ((this.x * 41) + y) * 41

    override def toString: String = s"Node($x, $y, $g, $h, $f, ${
      parent match {
        case Some(p) => p
        case None => "None"
      }
    })"
  }

  private def heuristic(begin: Node, end: Node): Double = {
    val (dx, dy) = ((begin.x - end.x).abs, (begin.y - end.y).abs)
    dx.min(dy) * DiagonalMoveCost + (dx - dy).abs * StraightMoveCost
  }

  private def reconstructPath(node: Node): List[Node] = {
    @tailrec
    def loop(current: Node, path: List[Node]): List[Node] = {
      current.parent match {
        case Some(parent) => loop(parent, current :: path)
        case None => current :: path
      }
    }

    loop(node, Nil)
  }

  private def neighbors(node: Node, grid: Array[Array[Byte]]): List[Node] = {
    val (mx, my) = (grid.length, grid(0).length)
    val directions: List[(Int, Int)] = List((1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))
    directions.flatMap { case (dx, dy) =>
      val (x, y) = (node.x + dx, node.y + dy)
      if (x < 0 || mx <= x || y < 0 || my <= y || grid(x)(y) == 0) None
      else {
        val cost = if (dx.abs == dy.abs) DiagonalMoveCost else StraightMoveCost
        Some(Node(x, y, node.g + cost, 0, Some(node)))
      }
    }
  }

  private def writeMap(): Unit = {
    val fos = new FileOutputStream("map")
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

  @tailrec
  final def aStar(): List[Node] = {
    if (open.isEmpty) Nil
    else {
      val current = open.dequeue()
      if (current == end) reconstructPath(current)
      else if (closed.contains(current)) aStar()
      else {
        closed += current
        neighbors(current, grid)
          .filter(neighbor => !closed.contains(neighbor))
          .map(neighbor => neighbor.copy(h = heuristic(neighbor, Node(end._1, end._2, 0, 0, None))))
          .foreach(open.enqueue(_))
        aStar()
      }
    }
  }
}

object AStar {
  def main(args: Array[String]): Unit = {
    Test3()
  }

  def Test2(): Unit = {
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
    val path = new AStar((2, 2), (10, 11), grid).aStar()
    path.foreach(node => println(s"${node.x}, ${node.y}"))
  }

  def Test3(): Unit = {
    val grid: Array[Array[Byte]] = Array(
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    )
    val path = new AStar((1, 1), (5, 8), grid).aStar()
  }
}
