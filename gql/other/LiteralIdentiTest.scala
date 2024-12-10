package other

object LiteralIdentiTest {
  def `yield`(a:Int):Int = a+1

  def main(args: Array[String]): Unit = {
    val x = 0
    val y = Symbol("aSymbol")
    System.out.println(y.name)
    System.out.println(`yield`(x))

  }
}


