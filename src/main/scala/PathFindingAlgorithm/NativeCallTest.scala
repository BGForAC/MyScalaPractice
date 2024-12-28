package PathFindingAlgorithm

object NativeCallTest extends App {
  System.loadLibrary("NativeCall")
  println(0xff000000)
  println(Integer.reverse(-16777216))
  System.out.println(new NativeCall().ffs(32))
  val num = 0.toLong << 32
  println(num)
  val num2 = -1.toLong << 35
  println(num2)
  val num3 = 0xafffffff.toLong
  println(num3)
  println(new NativeCall().clz(32))
  val a: Long = 12.toLong
  val x = -2147483648
//  println(System.getProperty("java.library.path"))
}
