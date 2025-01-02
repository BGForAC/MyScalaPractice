package mailSystem.entity

import java.util.Date

class PersonalMail extends Mail{
  private var sender: String = _
  private var receiver: String = _

  override def toString: String = {
    "PersonalMail{" +
      "sender='" + sender + '\'' +
      ", receiver='" + receiver + '\'' +
      ", mailId=" + mailId +
      ", content='" + content + '\'' +
      ", title='" + title + '\'' +
      ", attachment='" + attachment + '\'' +
      ", filter='" + filter + '\'' +
      ", publicTime=" + publicTime +
      ", deadline=" + deadline +
      ", createTime=" + createTime +
      ", updateTime=" + updateTime +
      '}'
  }
}
