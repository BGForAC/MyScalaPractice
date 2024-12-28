package PathFindingAlgorithm

object NativeCallTest extends App {
  System.loadLibrary("NativeCall")
  println(0xff000000)
  println(Integer.reverse(-16777216))
  System.out.println(new NativeCall().ffs(32))
  println(new NativeCall().ffs(~32))
  println(new NativeCall().clz(32))
  val a: Long = 12.toLong
  val x = -2147483648
  println(((-1 | 0) - 1) >>> 27)
  println(((1 | 0) - 1) >>> 27)
  println( ~(-1 << 5))

//  println(System.getProperty("java.library.path"))
}
