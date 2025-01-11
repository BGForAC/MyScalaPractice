package mailSystem

import akka.actor.ActorRef
import mailSystem.entity.{Item, Mail, PersonalMail, SystemMail}

object Messages {

  case class RequestGetClientActor(playerId: Long)

  case class RequestAddPlayer(playerId: Long, client: ActorRef)

  case class RequestDelPlayer(playerId: Long)

  case class RequestSendMail(sender: Long, receiver: Long, mail: PersonalMail)

  case class SendSystemMail(mail: SystemMail)

  case class RequestLoadMails(playerId: Long)

  case class RequestLoadItems(playerId: Long)

  case class RequestReadMail(playerId: Long, mailId: Long)

  case class RequestCollectAttachment(playerId: Long, mailId: Long)

  case class RequestDelMail(playerId: Long, mailId: Long)

  case class ReceiveReport(msg: String)

  case class ReceiveMails(mails: List[Mail])

  case class ReceiveItems(items: Map[Item, Int])

  case class ReceiveObtainItems(items: Map[Item, Int], mailId: Long)

  case class ReceiveCollectSuccess(mailId: Long)

  case class ReceiveDeleteSuccess(mailId: Long)

  case class Terminate()

  case class Log(content: String)
}
