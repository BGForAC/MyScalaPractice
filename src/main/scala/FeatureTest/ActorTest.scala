package FeatureTest

import akka.actor._

import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

case class Mail(content: String)
case class AddClient(clientActor: ActorRef)

class ServerActor(clientActors: mutable.Set[ActorRef]) extends Actor {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(
    new ThreadPoolExecutor(
      10,
      10,
      0L,
      java.util.concurrent.TimeUnit.MILLISECONDS,
      new java.util.concurrent.LinkedBlockingQueue[Runnable](),
      Executors.defaultThreadFactory(),
      new java.util.concurrent.ThreadPoolExecutor.AbortPolicy()
    )
  )

  override def receive: Receive = {
    case Mail(content) =>
      println("发送邮件", content)
      clientActors.foreach{ client =>
        Future {
          client ! Mail(content)
        }.onComplete{
          case Success(_) => println("发送成功")
          case Failure(exception) => println("发送失败", exception)
        }
      }
    case AddClient(clientActor) =>
      println("添加客户端")
      clientActors += clientActor
  }
}

class ClientActor(address: String, port: String) extends Actor {
  override def receive: Receive = {
    case Mail(content) =>
      println("接收邮件", content)
      println(content)
  }
}

object ActorTest extends App {
  private val system = ActorSystem("MyActorSystem")

  val clientActor = system.actorOf(Props(new ClientActor("127.0.0.1", "28234")), name = "clientActor")
  val clientActor2 = system.actorOf(Props(new ClientActor("127.0.0.1", "23718")), name = "clientActor2")

  val serverActor = system.actorOf(Props(new ServerActor(mutable.Set[ActorRef]())), name = "serverActor")

  serverActor ! AddClient(clientActor)
  serverActor ! AddClient(clientActor2)

  serverActor ! Mail("hello")

  system.terminate()
}
