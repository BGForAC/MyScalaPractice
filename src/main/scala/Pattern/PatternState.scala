package Pattern

object PatternState extends App {
  val context = new MyContext()

  StartState.doAction(context)
  context.doAction()
  StopState.doAction(context)
  context.doAction()
}

trait State {
  def doAction(context: MyContext): Unit
}

class MyContext {
  private var state: State = _

  def setState(state: State): Unit = this.state = state

  def getState: State = this.state

  def doAction(): Unit = state.doAction(this)
}

object StartState extends State {
  override def doAction(context: MyContext): Unit = {
    context.setState(this)
    println("in start state")
  }
}

object StopState extends State {
  override def doAction(context: MyContext): Unit = {
    context.setState(this)
    println("in stop state")
  }
}