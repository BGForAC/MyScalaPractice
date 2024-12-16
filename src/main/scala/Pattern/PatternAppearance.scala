package Pattern

object PatternAppearance extends App{
  new Facade().operation()
}

class SubSystemA {
  def operationA(): Unit = {
    println("SubsystemA operation")
  }
}

class SubSystemB {
  def operationB(): Unit = {
    println("SubsystemB operation")
  }
}

class SubSystemC {
  def operationC(): Unit = {
    println("SubsystemC operation")
  }
}

class Facade {
  private val subSystemA = new SubSystemA()
  private val subSystemB = new SubSystemB()
  private val subSystemC = new SubSystemC()

  def operation(): Unit = {
    subSystemA.operationA()
    subSystemB.operationB()
    subSystemC.operationC()
  }
}