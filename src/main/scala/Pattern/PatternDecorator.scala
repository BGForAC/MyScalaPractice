package Pattern

object PatternDecorator {
  trait IEquip {
    def description: String
    def getAttack: Double
  }

  trait IDecorator extends IEquip

  class WindBoots extends IEquip {
    override def description: String = "疾风靴子"

    override def getAttack: Double = 15
  }

  class DragonSlayer extends IEquip {
    override def description: String = "屠龙宝刀"

    override def getAttack: Double = 50
  }

  class RedGem(equip: IEquip) extends IDecorator {
    override def getAttack: Double = 15 + equip.getAttack

    override def description: String = "红宝石 + " + equip.description
  }

  class BlueGem(equip: IEquip) extends IDecorator {
    override def getAttack: Double = 30 + equip.getAttack

    override def description: String = "蓝宝石 + " + equip.description
  }

  class YellowGem(equip: IEquip) extends IDecorator {
    override def getAttack: Double = 45 + equip.getAttack

    override def description: String = "黄宝石 + " + equip.description
  }

  def main(args: Array[String]): Unit = {
    val equipment1 = new BlueGem(new BlueGem(new RedGem(new WindBoots)))
    val equipment2 = new YellowGem(new YellowGem(new YellowGem(new DragonSlayer)))
    println(s"攻击力：${equipment1.getAttack}")
    println(s"描述：${equipment1.description}")
    print("\n\n")
    println(s"攻击力：${equipment2.getAttack}")
    println(s"描述：${equipment2.description}")
  }
}
