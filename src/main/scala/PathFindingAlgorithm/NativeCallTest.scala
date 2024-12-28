package PathFindingAlgorithm

object NativeCallTest extends App {
  System.loadLibrary("NativeCall")
  println(0xff000000)
  println(Integer.reverse(-16777216))
  System.out.println(new NativeCall().ffs(-1))
  System.out.println(new NativeCall().ffs(0xffffffff << 30))
  val x = -2147483648
  println()
//  println(System.getProperty("java.library.path"))
}
