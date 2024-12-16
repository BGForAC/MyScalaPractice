package other

object Solution {
  def main(args: Array[String]): Unit = {
    System.out.println("Hello, World!")
    val x = 0
    val y = 0
    val node = new Node(x, y)
    System.out.println(+(+(+(+node))).myx)
  }

  class Node(x:Int,y:Int){
    val myx = x

    def unary_+ : Node = {
      new Node(myx+1,y)
    }
  }

  def numberOfAlternatingGroups(colors: Array[Int], k: Int): Int = {
    val extendColors=colors ++ colors
    colors.indices.count(i => (i to i+k-2).forall(i => extendColors(i)!=extendColors(i+1)))
  }
}
