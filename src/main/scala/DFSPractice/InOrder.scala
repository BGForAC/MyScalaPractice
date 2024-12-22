package DFSPractice

object InOrder extends App {
  val root = TreeNode(4, TreeNode(2, TreeNode(1, null, null), TreeNode(3, null, null)), TreeNode(6, TreeNode(5, null, null), TreeNode(7, null, null)))
  val result = inOrder(root)
  println(result)

  private def inOrder(node: InOrder.TreeNode): List[Int] = {
    if (node == null) Nil
    else inOrder(node.left) ++ List(node.value) ++ inOrder(node.right)
  }

  case class TreeNode(value: Int, left: TreeNode, right: TreeNode)
}


