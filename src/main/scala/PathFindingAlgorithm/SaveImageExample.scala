package PathFindingAlgorithm

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{File, IOException}
import javax.imageio.ImageIO


class SaveImageExample {
  var cols: Int = 466
  var rows: Int = 375
  var offsetZ: Int = 0
  var offsetX: Int = 0
  var SIDE_LENGTH: Int = 0
  var length: Int = 0
  var sx: Int = 0
  var sy: Int = 0
  final val OBSTACLE = 0
  final val COLOR_BEGIN = Color.BLUE
  final val COLOR_END = Color.BLUE
  final val COLOR_PATH = Color.RED
  final val COLOR_OPEN = Color.YELLOW
  final val COLOR_CLOSE = Color.CYAN
  final val COLOR_LINK = Color.GREEN
  final val COLOR_OBSTACLE = Color.BLACK
  final val COLOR_EMPTY = Color.WHITE


  var ex = 0
  var ey = 0


  def test2(env: Array[Array[Byte]], closedNodes: Array[(Int, Int)], openNodes: Array[(Int, Int)], path: List[(Int, Int)], outPutName: String): Unit = { //blocks:Array[Node],

    // 创建一个BufferedImage对象
    val image = new BufferedImage(2000, 2000, BufferedImage.TYPE_INT_RGB)
    // 获取Graphics2D对象，用于绘制
    val g2d = image.createGraphics
    // 绘制一些内容

//    g2d.setColor(Color.RED)
//    g2d.fillRect(10, 10, 100, 100)


    val position: Array[Int] = new Array[Int](2)

    val row = env.length
    val col = env(0).length
    for (i <- 0 until row) {
      for (j <- 0 until col) {
        if (env(i)(j) == OBSTACLE) {
          g2d.setColor(COLOR_OBSTACLE)
          g2d.fillRect(i * 4, j * 4, 4, 4)
        } else {
          g2d.setColor(COLOR_EMPTY)
          g2d.fillRect(i * 4, j * 4, 4, 4)
        }
      }
    }

    //closed
    g2d.setColor(COLOR_CLOSE)
    if (null != closedNodes)
      closedNodes.foreach(node => {
        g2d.fillRect(node._1 * 4, node._2 * 4, 4, 4)
      })

    //open
    g2d.setColor(COLOR_OPEN)
    if (null != openNodes)
      openNodes.foreach(node => {
        g2d.fillRect(node._1 * 4, node._2 * 4, 4, 4)
      })

    //起点
    g2d.setColor(COLOR_BEGIN)
    if (null != path)
      g2d.fillRect(path.head._1 * 4, path.head._2 * 4, 10, 10)

    //路径
    g2d.setColor(COLOR_PATH)
    if (null != path)
      for (i <- 0 until path.length - 1) {
        g2d.drawLine(path(i)._1 * 4 + 2, path(i)._2 * 4 + 2, path(i + 1)._1 * 4 + 2, path(i + 1)._2 * 4 + 2)
        //      println(path(i)._1 + " " + path(i)._2)
      }

    //终点
    g2d.setColor(COLOR_END)
    if (null != path)
      g2d.fillRect(path.last._1 * 4, path.last._2 * 4, 10, 10)
    //    g2d.setColor(Color.BLUE)
    //    g2d.fillRect(153*4,70*4,30,30)

    //连线
    g2d.setColor(COLOR_LINK)

    @scala.annotation.tailrec
    def linkPath(index: Int, count: Int): Unit = {
      if (index + 1 < path.length && count < 2000) {
        g2d.drawLine(path(index)._1 * 4 + 2, path(index)._2 * 4 + 2, path(index + 1)._1 * 4 + 2, path(index + 1)._2 * 4 + 2)
        linkPath(index + 1, count + 1)
      }
    }
    linkPath(0, 0)
//    if (null != path2)
//      while (path2 != path2.parrent && count < 2000) {
//        g2d.drawLine(path2.x * 4 + 2, path2.y * 4 + 2, path2.parrent.x * 4 + 2, path2.parrent.y * 4 + 2)
//        path2 = path2.parrent
//        count += 1
//      }
//    g2d.setColor(Color.GREEN)
//    g2d.fillRect(139 * 4, 254 * 4, 4, 4)
//    g2d.fillRect(138 * 4, 204 * 4, 4, 4) //

    // 释放Graphics2D对象
    g2d.dispose()
    // 保存图像到文件
    try {
      val outputFile = new File(outPutName + ".png")
      ImageIO.write(image, "png", outputFile)
      System.out.println("图像已保存到: " + outputFile.getAbsolutePath)

    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }


  def analyzeIndex(index: Int, position: Array[Int]): Unit = {
    position(0) = index % cols // y
    position(1) = index / cols // x
  }
}