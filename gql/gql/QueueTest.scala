package gql

import scala.collection.immutable.Queue

object QueueTest extends App{
  val empty=Queue(1,2,3,4)

  val newEmpty=empty.enqueue(1)

  empty.foreach(println)

  newEmpty.foreach(println)
}
