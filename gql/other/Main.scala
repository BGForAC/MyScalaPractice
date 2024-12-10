package other

case class Node(x: Int, y: Int, g: Double, h: Double, parent: Option[Node]) {
  def f: Double = g + h
}

object AStar {
  def heuristic(a: Node, b: Node): Double = {
    Math.abs(a.x - b.x) + Math.abs(a.y - b.y)
  }

  def neighbors(node: Node, grid: Array[Array[Int]]): List[Node] = {
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

  def reconstructPath(node: Node): List[Node] = {
    def loop(current: Node, path: List[Node]): List[Node] = {
      current.parent match {
        case Some(parent) => loop(parent, current :: path)
        case None => current :: path
      }
    }
    loop(node, Nil)
  }

  def aStar(start: Node, goal: Node, grid: Array[Array[Int]]): List[Node] = {
    val openSet = scala.collection.mutable.PriorityQueue[Node]()(Ordering.by(-_.f))
    val closedSet = scala.collection.mutable.Set[Node]()
    openSet.enqueue(start)

    while (openSet.nonEmpty) {
      val current = openSet.dequeue()
      if (current.x == goal.x && current.y == goal.y) {
        return reconstructPath(current)
      }

      closedSet.add(current)
      for (neighbor <- neighbors(current, grid)) {
        if (!closedSet.contains(neighbor)) {
          val tentativeG = current.g + 1
          if (!openSet.exists(n => n.x == neighbor.x && n.y == neighbor.y && n.g <= tentativeG)) {
            openSet.enqueue(neighbor.copy(g = tentativeG, h = heuristic(neighbor, goal)))
          }
        }
      }
    }
    List.empty
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
    val path = aStar(start, goal, grid)
    path.foreach(node => println(s"(${node.x}, ${node.y})"))
  }
}