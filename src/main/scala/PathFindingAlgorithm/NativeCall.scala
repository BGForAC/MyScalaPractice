package PathFindingAlgorithm

class NativeCall {
  @native def ffs(value: Int): Int
  @native def clz(value: Int): Int
  @native def ffsl(value: Long): Int
  @native def clzl(value: Long): Int
  @native def ffsll(value: Long): Int
  @native def clzll(value: Long): Int
}

object NativeCall {
}
