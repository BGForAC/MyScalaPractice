package mailSystem.entity

trait Mail {
  protected var mailId: Long = _
  protected var content: String = _
  protected var title: String = _
  protected var attachment: String = _
  protected var filter: String = _
  protected var publicTime: java.util.Date = _
  protected var deadline: java.util.Date = _
  protected var createTime: java.util.Date = _
  protected var updateTime: java.util.Date = _

  override def toString: String = {
    "Mail{" +
      "mailId=" + mailId +
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
