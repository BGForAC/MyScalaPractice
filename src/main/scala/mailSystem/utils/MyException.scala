package mailSystem.utils

object MyException {
  case class MailCountExceedException(message: String) extends Exception(message)

  case class MailNotExistException(message: String) extends Exception(message)

  case class MailSendFailException(message: String) extends Exception(message)

  case class MailReceiveFailException(message: String) extends Exception(message)

  case class MailDeleteFailException(message: String) extends Exception(message)

  case class PlayerNotExistException(message: String) extends Exception(message)

  case class PlayerAlreadyOnlineException(message: String) extends Exception(message)

  case class PlayerNotOnlineException(message: String) extends Exception(message)
}
