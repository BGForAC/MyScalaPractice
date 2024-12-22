package other

import scala.annotation.tailrec
import scala.collection.mutable

object MyAStar {
  case class Node(x: Int, y: Int, g: Double, h: Double, parent: Option[Node]) extends Ordered[Node] {
    def f: Double = g + h

    override def compare(that: Node): Int = (this.f - that.f).toInt

    def equalsPosition(obj: Any): Boolean = obj match {
      case obj: Node => obj.x == this.x && obj.y == this.y
      case _ => false
    }

    override def toString: String = {
      s"Node($x, $y, $g, $h)"
    }
  }

  def heuristic(a: Node, b: Node): Double = {
    val (dx, dy) = (a.x - b.x, a.y - b.y)
    dx * dx + dy * dy
  }

  def reconstructPath(node: Node): List[Node] = {
    @tailrec
    def loop(current: Node, path: List[Node]): List[Node] = {
      current.parent match {
        case Some(parent) => println(parent); loop(parent, current :: path)
        case None => current :: path
      }
    }

    loop(node, Nil)
  }

  private def neighbors(node: Node, grid: Array[Array[Int]]): List[Node] = {
    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))
    directions.flatMap { case (dx, dy) =>
      val newX = node.x + dx
      val newY = node.y + dy
      if (newX >= 0 && newX < grid.length && newY >= 0 && newY < grid(0).length && grid(newX)(newY) == 0) {
        Some(Node(newX, newY, node.g + 1, 0, Some(node)))
      } else {
        None
      }
    }
  }

  implicit val nodeOrdering: Ordering[Node] = Ordering.by(-_.f)

  def aStar(grid: Array[Array[Int]], start: Node, end: Node): List[Node] = {
    val openSet = new mutable.PriorityQueue[Node]()
    val closedSet = new mutable.HashSet[Node]
    openSet.enqueue(start)

    @tailrec
    def f(): List[Node] = {
      if (openSet.isEmpty) Nil
      else {
        val current = openSet.dequeue()
        if (current equalsPosition end) reconstructPath(current)
        else {
          closedSet.add(current)
          neighbors(current, grid)
            .filter(neighbor => {
              !closedSet.contains(neighbor) &&
                !openSet.exists(n => (n equalsPosition neighbor) && n.g <= current.g + 1)
            }).map {
              neighbor => neighbor.copy(g = current.g + 1, h = heuristic(neighbor, end))
            }.foreach(openSet.enqueue(_))
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
    val path = aStar(grid, start, goal)
    path.foreach(node => println(s"(${node.x}, ${node.y})"))
  }
}
