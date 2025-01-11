package mailSystem

import mailSystem.dao.MyGlobal
import mailSystem.entity.{Item, Mail, PersonalMail, SystemMail}
import mailSystem.service.MailService.deletedMails
import mailSystem.service.{ItemService, MailService, PlayerService}
import mailSystem.utils.{JedisHelper, MapBean, MapBeanUtils}

import java.sql.{SQLException, SQLIntegrityConstraintViolationException}
import java.time.LocalDateTime
import scala.collection.convert.ImplicitConversions.`map AsScala`
import scala.collection.mutable

/**
 * 这个类用于提供邮件系统的外部接口的实现
 */
object MailSystemImpl {
  private def itemId2Key(itemId: Long): String = s"item:$itemId"

  private def playerId2Key(playerId: Long): String = s"player:$playerId"

  private def systemMailId2Key(systemMailId: Long): String = s"system_mail:$systemMailId"

  private def distributionKeyForAttachmentCollect(playerId: Long, mailId: Long): String = s"distribution:attachment:collect:$playerId:$mailId"

  private def distributionKeyForStatusChange: String = "distribution:status:change"

  private def keyForSystemMail = "system_mail"

  private def keyForMailsRead = "mails_read"

  private def keyForMailsCollect = "mails_collect"

  private def keyForItems = "items"

  private def key2PlayerId(key: String): Option[Long] = {
    key match {
      case key if key.startsWith("player:") => Some(key.split(":")(1).toLong)
      case _ => None
    }
  }

  private def myLog(content: String): Unit = {
    content match {
      case msg if msg.startsWith("process") => println(s"\u001B[32m $msg \u001B[0m")
      case _ => println(s"\u001B[31m $content \u001B[0m")
    }
  }

  // 查看玩家是否存在
  def findPlayer(playerId: Long): Option[Long] = {
    PlayerService.allPlayers().find(_.getPlayerId == playerId).map(_.getPlayerId)
  }

  // 删除客户端
  def deleteClient(playerId: Long): Unit = {
    JedisHelper.execute { jedis =>
      jedis.del(playerId2Key(playerId))
    }
  }

  // 与数据库进行同步，将数据库中的邮件加载到用户内存中
  def loadPlayersMail(playerId: Long): Either[String, List[Mail]] = {
    JedisHelper.execute { jedis =>
      if (jedis.hexists(playerId2Key(playerId), keyForMailsRead) && jedis.hexists(playerId2Key(playerId), keyForMailsCollect)) {
        myLog(s"process: 玩家 $playerId 的邮箱已经加载到内存中")
        Left("邮件已经被加载到内存中")
      } else if (jedis.hexists(playerId2Key(playerId), keyForMailsRead) && !jedis.hexists(playerId2Key(playerId), keyForMailsCollect)) {
        myLog(s"process: 玩家 $playerId 的邮箱已经加载到内存中，但是领取状态更新后被删除")
        val mailsCollect = PlayerService.getCollectStatus(playerId)
        jedis.hset(playerId2Key(playerId), keyForMailsCollect, mailsCollect)
        Left("领取状态已更新")
      } else {
        myLog(s"process: 玩家 $playerId 邮箱未加载到内存中")
        val mails = MailService.personalMails(playerId)
        mails ++= loadSystemMails()
        println(deletedMails(playerId))
        mails --= deletedMails(playerId)
        mails.toList match {
          case Nil => Left("用户没有邮件")
          case mails =>
            myLog(s"process: 玩家 $playerId 邮箱加载成功")
            // 讲邮件已读已领取状态存入redis，将邮件存入用户内存
            val player = PlayerService.getPlayer(playerId)
            val mailsRead = player.getMailsRead
            val mailsCollect = player.getMailsCollect
            jedis.hset(playerId2Key(playerId), keyForMailsCollect, mailsCollect)
            jedis.hset(playerId2Key(playerId), keyForMailsRead, mailsRead)
            if (mailsRead.nonEmpty) {
              mailsRead.split(",").foreach { mailId =>
                mails.find(_.getMailId == mailId.toLong).foreach(_.setRead(true))
              }
            }
            if (mailsCollect.nonEmpty) {
              mailsCollect.split(",").foreach { mailId =>
                mails.find(_.getMailId == mailId.toLong).foreach(_.setCollect(true))
              }
            }
            Right(mails)
        }
      }
    }
  }

  // 加载系统邮件, 先从redis中加载，如果没有再从数据库中加载
  private def loadSystemMails(): mutable.ListBuffer[Mail] = {
    JedisHelper.execute { jedis =>
      val systemMails = mutable.ListBuffer[Mail]()
      val mails = jedis.hgetAll(keyForSystemMail)
      if (mails.isEmpty) {
        myLog("process: 系统邮件未加载到缓存中, 开始加载")
        systemMails ++= MailService.systemMails()
        systemMails.foreach { mail =>
          jedis.hset(keyForSystemMail, systemMailId2Key(mail.getMailId), mail.toString)
        }
      } else {
        myLog("process: 系统邮件已加载到缓存中")
        mails.foreach { case (_, mail) =>
          systemMails += MapBeanUtils.json2SystemMail(mail)
        }
      }
      systemMails
    }
  }

  // 获取物品信息, 用于显示, 先从redis中加载，如果没有再从数据库中加载
  def getItem(itemId: Long): Item = {
    JedisHelper.execute { jedis =>
      jedis.hget(keyForItems, itemId2Key(itemId)) match {
        case null =>
          myLog(s"process: 物品 $itemId 未加载到缓存中, 开始加载")
          val item = ItemService.getItem(itemId)
          jedis.hset("item", itemId.toString, item.toString)
          item
        case itemName =>
          myLog(s"process: 物品 $itemId 已加载到缓存中")
          MapBeanUtils.json2Item(itemName)
      }
    }
  }

  // 加载用户背包
  def loadPlayersItems(playerId: Long): Either[String, Map[Item, Int]] = {
    try {
      val items = PlayerService.getItems(playerId)
      Right(items.map { case (itemId, quantity) => (getItem(itemId), quantity) })
    } catch {
      case e: Exception =>
        Left("加载背包失败" + e.getMessage)
    }
  }

  // 增加个人邮件，收件人邮件数加一，使用触发器保证邮件数不超过上限
  def addPersonalMail(senderId: Long, receiverId: Long, mail: PersonalMail): Either[String, Mail] = {
    try {
      Right(MailService.addPersonalMail(senderId, receiverId, mail))
    } catch {
      case e: SQLException if e.getSQLState == MyGlobal.SQLERRORMAILCOUNTEXCEED =>
        myLog(s"process: 玩家 $receiverId 邮箱已满")
        Left("收件人邮箱已满发送失败")
      case e: IllegalArgumentException =>
        myLog(s"process: 玩家 $receiverId 邮件发送失败, 错误信息：${e.getMessage}")
        Left("邮件发送失败, 错误信息：" + e.getMessage)
      case e: SQLIntegrityConstraintViolationException =>
        myLog(s"process: 玩家 $receiverId 邮件发送失败, 用户不存在, 错误信息：${e.getMessage}")
        Left("邮件发送失败, 用户不存在, 错误信息：" + e.getMessage)
      case e: Exception =>
        myLog(s"process: 玩家 $receiverId 邮件发送失败， 未捕获异常" + e.getMessage)
        Left("邮件发送失败， 未捕获异常")
    }
  }

  // 将系统邮件存入数据库，并删除缓存
  def addSystemMail(mail: SystemMail): Either[String, SystemMail] = {
    try {
      var systemMail: SystemMail = null
      JedisHelper.execute { jedis =>
        systemMail = MailService.addSystemMail(mail)
        jedis.del(keyForSystemMail)
      }
      Right(systemMail)
    } catch {
      case e: IllegalArgumentException =>
        myLog(s"process: 系统邮件发送失败, 错误信息：${e.getMessage}")
        Left("系统邮件发送失败, 错误信息：" + e.getMessage)
      case e: Exception =>
        myLog(s"process: 系统邮件发送失败， 未捕获异常" + e.getMessage)
        Left("系统邮件发送失败， 未捕获异常")
    }
  }

  /*
   * 用户读取邮件，在redis中记录邮件已读状态，定时任务将状态同步到数据库
   * 读取不存在的邮件Id或者读取不存在的用户时可能会反复加载，需要修改，可以设置循环次数
   */
  def readMail(playerId: Long, mailId: Long): Unit = {
    JedisHelper.execute { jedis =>
      jedis.hget(playerId2Key(playerId), keyForMailsRead) match {
        case null =>
          myLog(s"process: 玩家 $playerId 的阅读情况可能未加载到缓存中, 尝试再次加载")
          loadPlayersMail(playerId)
          readMail(playerId, mailId)
        case mailsRead =>
          if (mailsRead.contains(mailId.toString)) myLog(s"玩家 $playerId 已经阅读过邮件 $mailId") else {
            myLog(s"process: 玩家 $playerId 阅读了邮件 $mailId")
            jedis.hset(playerId2Key(playerId), keyForMailsRead, s"$mailsRead$mailId,")
          }
      }
    }
  }

  /**
   * 用户领取附件，立刻同步到数据库。 领取附件时需要加锁，防止多个用户同时领取同一封邮件的附件
   * 附件领取成功后，将领取状态从redis中删除,保持一致性
   */
  def collectAttachment(playerId: Long, mailId: Long): Either[String, Map[Item, Int]] = {
    JedisHelper.execute { jedis =>
      val mailsCollect = jedis.hget(playerId2Key(playerId), keyForMailsCollect)
      if (mailsCollect != null && mailsCollect.contains(mailId.toString)) {
        myLog(s"process: 玩家 $playerId 已经领取过邮件 $mailId 的附件")
        return Left("已经领取过附件")
      }
    }

    val mail = MailService.getMail(mailId)
    if (mail == null) return Left("邮件不存在")
    mail match {
      case personalMail: PersonalMail if personalMail.receiverId != playerId => return Left("你不是收件人，无法领取附件")
      case _ =>
    }
    val attachmentJson = mail.getAttachment
    if (attachmentJson == "{}") return Left("邮件没有附件")
    val deadline = mail.getDeadline
    val publicTime = mail.getPublicTime
    if (deadline.isBefore(LocalDateTime.now)) return Left("邮件已过期")
    if (publicTime.isAfter(LocalDateTime.now)) return Left("邮件未到领取时间")

    val lockKey = distributionKeyForAttachmentCollect(playerId, mailId)
    val lockValue = System.currentTimeMillis().toString

    try {
      JedisHelper.executeWithDistributionLock(lockKey, lockValue, 20) { jedis =>
        val attachment = MapBean.toMutableMap(attachmentJson).toMap.asInstanceOf[Map[String, Int]]
        PlayerService.collectAttachment(playerId, mailId, attachment)
        jedis.hdel(playerId2Key(playerId), keyForMailsCollect)
        Right(attachment.map { case (itemId, quantity) => (ItemService.getItem(itemId.toLong), quantity) })
      }
    } catch {
      case e: IllegalStateException =>
        Left(e.toString)
      case e: Exception =>
        Left("领取附件失败" + e.getMessage)
    }
  }

  /*
   * 删除邮件，需要注意防止定时同步任务在删除数据库中的邮件信息后又把缓存中未删除的邮件阅读状态同步到数据库，导致删除失败
   * 目前没有想到更好的解决方案，只能在删除和同步时加上一样的分布式锁，保证同步任务不会导致被删除的邮件再次被同步
   */
  def deleteMail(playerId: Long, mailId: Long): Either[String, Unit] = {
    try {
      MailService.getMail(mailId) match {
        case mail: SystemMail =>
          myLog(s"process: 玩家 $playerId 尝试删除系统邮件 $mailId")
          JedisHelper.executeWithDistributionLock(distributionKeyForStatusChange, System.currentTimeMillis().toString, 20) { jedis =>
            jedis.del(playerId2Key(playerId))
            Right(MailService.deleteSystemMail(playerId, mail.getMailId))
          }
        case mail: PersonalMail if mail.senderId == playerId =>
          myLog(s"process: 玩家 $playerId 尝试删除发送的邮件 $mailId")
          Right(MailService.deleteMailSend(playerId, mail.getMailId))
        case mail: PersonalMail if mail.receiverId == playerId =>
          myLog(s"process: 玩家 $playerId 尝试删除收到的邮件 $mailId")
          JedisHelper.executeWithDistributionLock(distributionKeyForStatusChange, System.currentTimeMillis().toString, 20) { jedis =>
            jedis.del(playerId2Key(playerId))
            Right(MailService.deleteMailReceive(playerId, mail.getMailId))
          }
        case _ =>
          myLog(s"process: 玩家 $playerId 删除邮件 $mailId 失败, 删除不合法")
          Left("删除邮件失败")
      }
    } catch {
      case e: Exception =>
        Left("删除邮件失败" + e.getMessage)
    }
  }

  // 定时任务，同步邮件已读状态到数据库
  def syncMailsRead(): Unit = {
    val key = distributionKeyForStatusChange
    val value = System.currentTimeMillis().toString

    myLog("process: 同步邮件已读状态")
    try {
      JedisHelper.executeWithDistributionLock(key, value, 20) { jedis =>
        val players: Iterable[Long] = jedis.keys("*").toArray.flatMap { key => key2PlayerId(key.asInstanceOf[String]) }
        players.foreach { playerId =>
          jedis.hget(playerId2Key(playerId), keyForMailsRead) match {
            case null =>
              myLog(s"process: 玩家 $playerId 的邮件已读状态未加载到缓存中")
              loadPlayersMail(playerId)
            case mailsRead =>
              if (mailsRead.isEmpty) {
                myLog(s"process: 玩家 $playerId 的邮件已读状态为空")
              } else {
                myLog(s"process: 玩家 $playerId 的邮件已读状态同步到数据库")
                PlayerService.updateMailsRead(playerId, mailsRead)
              }
          }
        }
      }
    } catch {
      case e: Exception =>
        myLog(e.getMessage)
        myLog("process: 同步邮件已读状态失败")
    }
  }
}
