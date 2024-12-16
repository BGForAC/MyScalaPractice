package Solutions

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def maxPathSum(root: TreeNode): Int = {

    def maxRoute(root: TreeNode): Int = {
      if (root == null) 0
      else (maxRoute(root.left) max 0 max maxRoute(root.right)) + root.value
    }
    
    if (root == null) 1 << 31
    else {
      val sum = (maxRoute(root.left) max 0) + root.value + (maxRoute(root.right) max 0)
      sum max maxPathSum(root.left) max maxPathSum(root.right)
    }
  }

}