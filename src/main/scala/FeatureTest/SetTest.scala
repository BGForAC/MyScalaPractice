package FeatureTest

object SetTest extends App{
  val set=Set(1,2,3)
  val muSet=scala.collection.mutable.Set(1,2,3)

  val newSet=set & Set(1,5,3)

  newSet.foreach(println)


}