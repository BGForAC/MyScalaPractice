package BFSPractice

import scala.annotation.tailrec

object TraversalTree extends App {
  val root = TreeNode(1, TreeNode(2, TreeNode(4, null, null), TreeNode(5, null, null)), TreeNode(3, TreeNode(6, null, null), TreeNode(7, null, null)))
  val result = levelOrder(root)
  println(result)

  private def levelOrder(node: TraversalTree.TreeNode): List[Int] = {
    levelOrderTraversal (if (node == null) Nil else List(root), Nil)
  }

  @tailrec
  private def levelOrderTraversal(root: List[TreeNode], res: List[Int]): List[Int] = {
    if (root.isEmpty) res
    else levelOrderTraversal(root.flatMap(node => List(node.left, node.right)).filter(_ != null), res ++ root.map(_.value))
  }

  case class TreeNode(value: Int, left: TreeNode, right: TreeNode)
}

