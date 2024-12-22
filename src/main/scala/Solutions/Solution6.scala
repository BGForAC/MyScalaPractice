package Solutions

import scala.collection.mutable

class Solution6 {
  def minValidStrings(words: Array[String], target: String): Int = {
    buildTree(words)
    minAviStrLength(target)
  }

  def buildTree(words: Array[String]) = {
    words.foreach {
      root.memory(_)
    }
  }

  val root = TreeNode()

  val memo = new mutable.HashMap[String, Int]()
  memo += ("" -> 0)

  def minAviStrLength(target: String): Int = {
    if (!memo.contains(target)) {
      var prefix = 1
      var ans = 1 << 30
      while (root.exists(target.slice(0, prefix))) {
        ans = (minAviStrLength(target.slice(prefix, target.length)) + 1 min ans)
        prefix += 1
      }
      memo += (target -> ans)
    }
    memo.get(target).get
  }

  case class TreeNode() {
    val children = new mutable.HashMap[Char, TreeNode]

    def setChild(index: Char): TreeNode = {
      this.children.getOrElse(index, {
        children += (index -> TreeNode())
        children.get(index).get
      })
    }

    def exists(str: String): Boolean = {
      var curRoot: TreeNode = this
      var isExist = true
      for (c <- str){
        if (!curRoot.children.contains(c)) isExist = false
        curRoot = curRoot.children.get(c).get
      }
      isExist
    }

    def memory(str: String): Unit = {
      var curRoot = this
      str.foreach{
        char => curRoot = curRoot.setChild(char)
      }
    }
  }

}


