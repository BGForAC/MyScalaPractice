package gql

import gql.Element.{ArrayElement, elem}

object Spiral extends App{
  val a=List(1,2,3)
  a.foreach(println)
  val space=elem(" ")
  val corner=elem("+")
  def spiral(nEdges:Int,direction:Int):Element={
    if (nEdges == 1) corner
    else{
      val sp=spiral(nEdges-1,(direction+3)%4)
      val verticalBar=elem('|',1,sp.height)
      val horizontalBar=elem('-',sp.width,1)
      if (direction==0){
        (corner beside horizontalBar) above (sp beside space)
      }else if (direction==1){
        (sp above space) beside (corner above verticalBar)
      }else if (direction==2){
        (space beside sp) above (horizontalBar beside corner)
      }else{
        (verticalBar above corner) beside (space above sp)
      }
    }
  }

  val nEdges=11
  println(spiral(nEdges,0))
}

abstract class Element {
  def contents: Array[String]

  def height: Int = contents.length

  def width: Int = if (height == 0) 0 else contents(0).length

  def above(that:Element):Element={
    val this1=this widen that.width
    val that1=that widen this.width
    new ArrayElement(this1.contents ++ that1.contents)
  }

  def beside(that:Element):Element={
    val this1=this heighten that.height
    val that1=that heighten this.height
    new ArrayElement(for ((line1,line2) <- this1.contents zip that1.contents) yield line1+line2)
  }

  def widen(w:Int):Element={
    if (w<=this.width) this
    else{
      val left=(w-this.width)/2
      val right=w-this.width-left
      elem(' ',left,height) beside this beside elem(' ',right,height)
    }
  }

  def heighten(h:Int):Element={
    if (h<=this.height) this
    else{
      val top=elem(' ',width,((h-this.height)/2))
      val bottom=elem(' ',width, (h-this.height-top.height))
      top above this above bottom
    }
  }

  override def toString: String =contents.mkString("\n")
}

object Element {
  class ArrayElement(val contents: Array[String]) extends Element

  class LineElement(s: String) extends Element{
    def contents: Array[String] = Array(s)
  }

  class UniformElement(
                        ch: Char,
                        override val width: Int,
                        override val height: Int
                      ) extends Element {
    private val line=ch.toString*width
    override def contents: Array[String] = Array.fill(height)(line)
  }

  def elem(contents:Array[String]):Element=new ArrayElement(contents)

  def elem(s:String):Element=new LineElement(s)

  def elem(ch:Char,width:Int,height:Int):Element=new UniformElement(ch, width, height)
}
