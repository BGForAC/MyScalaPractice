package FeatureTest

import akka.actor._

class MyActor extends Actor {
  override def receive: Receive = {
    case "hh" => println("hello")
    case _ => println("world")
  }
}

object ActorTest extends App {
  private val system = ActorSystem("MyActorSystem")
  private val myActor = system.actorOf(Props[MyActor], name = "MyActor")
  myActor ! "hh"
  myActor ! "world"
  system.terminate()
}
