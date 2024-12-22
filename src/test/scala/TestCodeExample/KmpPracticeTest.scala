package TestCodeExample

import TestCodeExample.KmpPractice.kmpMatcher
import org.scalatest.funsuite.AnyFunSuite

class KmpPracticeTest extends AnyFunSuite{
  test("testKmp") {

    // 测试数据1
    val result1 = kmpMatcher("abababababa", "ab").toList
    assert(result1 == List(0, 2, 4, 6, 8), "第一组测试数据出错")

    // 测试数据2
    val result2 = kmpMatcher("ABABDABACDABABCABAB", "ABABCABAB").toList
    assert(result2 == List(10), "第二组测试数据出错")

    // 测试数据3
    val result3 = kmpMatcher("hello world", "world").toList
    assert(result3 == List(6), "第三组测试数据出错")

    // 测试数据4
    val result4 = kmpMatcher("aaaaa", "aa").toList
    assert(result4 == List(0, 1, 2, 3), "第四组测试数据出错")

    // 测试数据5
    val result5 = kmpMatcher("mississippi", "iss").toList
    assert(result5 == List(1, 4), "第五组测试数据出错")
  }
}
