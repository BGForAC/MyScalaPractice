package FeatureTest

import scala.collection.immutable

object MergeSort extends App{
  def mSort[T](less:(T,T)=>Boolean)(xs:List[T]):List[T]={
    def merge(xs:List[T],ys:List[T]):List[T]= (xs,ys) match {
      case (_,immutable.Nil) => xs
      case (immutable.Nil,_) => ys
      case (xs,ys) => if (less(xs.head,ys.head)) xs.head :: merge(xs.tail,ys) else ys.head :: merge(ys.tail,xs)
    }

    val mid=xs.length>>1
    if (mid==0) xs
    else {
      val (pre,after) = xs.splitAt(mid)
      merge(mSort(less)(pre),mSort(less)(after))
    }
  }

  val intSort=mSort((x:Int,y:Int)=>x<y)_
  val a=mSort((x:Int,y:Int)=>x<y)(List(45,48,81,12,32,45,82,11))
  val b=intSort(List(45,48,81,12,32,45,82,11))
  a.foreach(println)
  b.foreach(println)
}
