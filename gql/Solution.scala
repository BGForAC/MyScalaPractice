import scala.annotation.tailrec

object Solution {
  def findLadders(beginWord: String, endWord: String, wordList: List[String]): List[List[String]] = {
    if (!wordList.contains(endWord)) Nil
    else findSeq(beginWord,List(List(endWord)),(beginWord :: wordList).distinct)
  }

  @tailrec
  def findSeq(beginWord: String, retList: List[List[String]],wordList: List[String]):List[List[String]]={
    val newRetList:List[List[String]]=retList.flatMap(list =>
      wordList
        .filter(ele => !list.contains(ele) && difCount(list.head,ele) == 1 && list.head != beginWord)
        .map(_::list))
    val filteredList=newRetList.filter(_.head == beginWord)
    if (retList == newRetList || filteredList.nonEmpty) filteredList else findSeq(beginWord,newRetList,wordList)
  }

  def difCount(s1: String,s2: String): Int = {
    (0 until s1.length).count(i => s1(i) != s2(i))
  }
}