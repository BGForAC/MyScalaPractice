package Pattern

object PatternBuilder {

  class ICharacter(head: String, body: String, leg: String) {
    override def toString: String = s"头部是$head，身体是$body，腿部是$leg"
  }

  trait Builder {
    protected var head: String = _
    protected var body: String = _
    protected var leg: String = _

    def buildHead(): Builder
    def buildBody(): Builder
    def buildLeg(): Builder
    def build(): ICharacter
  }

  class WarriorBuilder extends Builder {

    override def buildHead(): Builder = {
      head = "战士头盔"
      this
    }

    override def buildBody(): Builder = {
      body = "战士胸甲"
      this
    }

    override def buildLeg(): Builder = {
      leg = "战士护腿"
      this
    }

    override def build(): ICharacter = new ICharacter(head, body, leg)
  }

  class MagicianBuilder extends Builder {

    override def buildHead(): Builder = {
      head = "法师帽子"
      this
    }

    override def buildBody(): Builder = {
      body = "法师长袍"
      this
    }

    override def buildLeg(): Builder = {
      leg = "法师护腿"
      this
    }

    override def build(): ICharacter = new ICharacter(head, body, leg)
  }

  class Director(builder: Builder) {
    def construct: ICharacter = builder.buildHead().buildBody().buildLeg().build()
  }

  def main(args: Array[String]): Unit = {
    print(new Director(new WarriorBuilder).construct)
    print("\n")
    print(new Director(new MagicianBuilder).construct)
  }
}
