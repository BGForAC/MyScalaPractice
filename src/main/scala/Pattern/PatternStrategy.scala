package Pattern

object PatternStrategy extends App {
  println(new Context(new OperationAdd()).operation(10, 5))
  println(new Context(new OperationMulti()).operation(10, 5))
  println(new Context(new OperationSub()).operation(10, 5))
  println(new Context(new OperationDiv()).operation(10, 5))
}

class Context(strategyPara: Strategy){
  private var strategy = strategyPara

  def operation(a: Int, b: Int): Int = strategy.operation(a, b)
}

trait Strategy {
  def operation(a: Int, b: Int): Int
}

class OperationAdd extends Strategy {
  override def operation(a: Int, b: Int): Int = a + b
}

class OperationMulti extends Strategy {
  override def operation(a: Int, b: Int): Int = a * b
}

class OperationSub extends Strategy {
  override def operation(a: Int, b: Int): Int = a - b
}

class OperationDiv extends Strategy {
  override def operation(a: Int, b: Int): Int = a / b
}