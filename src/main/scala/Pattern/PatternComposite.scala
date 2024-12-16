package Pattern

import scala.collection.mutable.ListBuffer

object PatternComposite extends App {
  val f1 = new MyFiles("file1")
  val f2 = new MyFiles("file2")
  val f3 = new MyFiles("file3")
  val f4 = new MyFiles("file4")
  val f5 = new MyFiles("file5")
  val f6 = new MyFiles("file6")

  val d1 = new MyDirectory("dir1")
  val d2 = new MyDirectory("dir2")
  val d3 = new MyDirectory("dir3")

  d1.add(f1)
  d1.add(f2)
  d1.add(d2)
  d2.add(f3)
  d2.add(f4)
  d2.add(d3)
  d3.add(f5)
  d3.add(f6)
  d1.showDetails("")
}

trait FileSystemComponent {
  def showDetails(prefix: String): Unit
}

class MyFiles(name: String) extends FileSystemComponent {
  override def showDetails(prefix: String): Unit = println(s"${prefix}File:$name")
}

class MyDirectory(name: String) extends FileSystemComponent {
  private val children = ListBuffer[FileSystemComponent]()

  def add(component: FileSystemComponent): Unit = {
    children += component
  }

  def remove(component: FileSystemComponent): Unit = {
    children -= component
  }

  override def showDetails(prefix: String): Unit = {
    println(s"${prefix}Directory:$name")
    children.foreach { child =>
      child.showDetails(prefix + "\t")
    }
  }
}
