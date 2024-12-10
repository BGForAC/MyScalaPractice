package other

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

object MyAStar {
  case class Node(x: Int, y: Int, g: Double, h: Double, parent: Option[Node]) {
    def f: Double = g + h
  }

  def heuristic(a: Node, b: Node): Double = {
    val dy = b.y - a.y
    val dx = b.x - a.x
    dx * dx + dy * dy
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

  def aStar(grid: Array[Array[Int]], start: Node, end: Node): List[Node] = {
    val openSet = new HashSet[Node]
    val closed = new HashSet[Node]

    openSet + start

    while (openSet.nonEmpty) {

    }
    List.empty
  }

  def main(args: Array[String]) = {
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
