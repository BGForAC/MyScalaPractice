package mailSystem

import akka.actor._
import com.typesafe.config.ConfigFactory

import scala.collection.mutable

object MailSystemImitator {
  def createServer(): (ActorSystem, ActorRef) = {
    val config = ConfigFactory.parseString(
      """
        |akka {
        |  actor {
        |    allow-java-serialization = on
        |    serialize-messages = on
        |    provider = "akka.remote.RemoteActorRefProvider"
        |  }
        |  remote {
        |    artery {
        |      canonical.hostname = "127.0.0.1"
        |      canonical.port = 2552
        |    }
        |  }
        |}
        |""".stripMargin)
    val system = ActorSystem("MyMailSystem", config)
    val serverActor = system.actorOf(Props(new ServerActor(mutable.Map[Long, ActorRef](), system)), "serverActor")
    (system, serverActor)
  }

  def main(args: Array[String]): Unit = {

    //    for (_ <- 1 to 1) addRandomClient()
    //    sleep(1000)
    //    val schedule = sendMailRegularly


    //    val schedule = readMailRegularly(2.seconds)
    //    val schedule2 = syncRead2DBRegularly(10.seconds)
    //    val schedule = sendMailRegularly(10.seconds)
    //    addConcreteClient(532125159977385984L)
    //    sleep(10000)
    //    delConcreteClient(532125159977385984L)
    //    sleep(10000)
    //    addConcreteClient(532125159977385984L)

    //    val schedule1 = sendSystemMailRegularly
    //    val schedule2 = addClientRegularly
    //    val schedule3 = deleteMailRegularly
    //    system.terminate()
  }
}
