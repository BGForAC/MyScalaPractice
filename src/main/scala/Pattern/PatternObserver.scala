package Pattern

import scala.collection.mutable.ListBuffer

object PatternObserver extends App {
  val subject = new Subject

  val octalObserver = new OctalObserver(subject)
  val binaryObserver = new BinaryObserver(subject)
  val hexObserver = new HexObserver(subject)

  subject.setState(10)
  subject.setState(20)
}

class Subject {
  private val observers = ListBuffer[Observer]()
  private var state: Int = 0

  def attach(observer: Observer): Unit = observers += observer

  def getState: Int = state

  def setState(state: Int): Unit = {
    this.state = state
    notifyAllObservers()
  }

  private def notifyAllObservers(): Unit = {
    observers.foreach(_.update())
  }
}

trait Observer {
  protected val subject: Subject

  def update(): Unit
}

class OctalObserver(subjectPara: Subject) extends Observer {
  val subject: Subject = subjectPara
  subject.attach(this)

  override def update(): Unit = println(s"octalUpdate:${subject.getState.toOctalString}")
}

class BinaryObserver(subjectPara: Subject) extends Observer {
  val subject: Subject = subjectPara
  subject.attach(this)

  override def update(): Unit = println(s"binaryUpdate:${subject.getState.toBinaryString}")
}

class HexObserver(subjectPara: Subject) extends Observer {
  val subject: Subject = subjectPara
  subject.attach(this)

  override def update(): Unit = println(s"hexUpdate:${subject.getState.toHexString}")
}