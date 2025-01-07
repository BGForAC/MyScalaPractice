package mailSystem.utils

import grizzled.slf4j.Logging
import org.apache.commons.lang3.StringUtils
import org.codehaus.jackson.map.ObjectMapper

import java.io.IOException
import java.sql.{Time, Timestamp}
import java.util
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
/*继承自HashMap自定义的一个数据结构，可将Json 格式转成MapBean格式
* 在ServerDef中使用*/
object MapBean extends Logging {
  final val EMPTY: MapBean = empty
  /*ObjectMapper 帮助我们快速的进行各个类型和Json类型的相互转换。*/
  final val objectMapper = new ObjectMapper

  def empty[String, Any] = new MapBean

  def apply(elems: (String, Any)*): MapBean = empty ++= elems

  //将JSON 字符串格式文件转换成MapBean
  def toMutableMap(obj: Any): MapBean = obj match {
    case mj: util.Map[String, _]@unchecked => toMutableMap(convertScala(mj))
    case m: mutable.Map[String, _]@unchecked => MapBean.empty ++= m
    case m: Map[String, _]@unchecked => MapBean.empty ++= m
    case jsonString: String =>
      if (StringUtils.isBlank(jsonString)) MapBean.empty
      else {
        try {
          val m = objectMapper.readValue(jsonString, classOf[util.Map[String, Any]])
          toMutableMap(m)
        } catch {
          case e: Exception =>
            error("error json:" + jsonString)
            MapBean.empty
        }
      }
    case null => MapBean.empty
    case _ => throw new IllegalArgumentException
  }

  private def convertScala(value: Any): Any = value match {
    case l: util.List[_]@unchecked =>
      l.asScala.map(convertScala).toList
    case s: util.Set[_]@unchecked =>
      s.asScala.map(convertScala).toSet
    case m: util.Map[String, _]@unchecked =>
      val map = MapBean.empty
      for ((k, v) <- m.asScala) {
        map += k -> convertScala(v)
      }
      map
    case _ =>
      value
  }
  import scala.language.implicitConversions


  def map2Bean[T](map: MapBean, clazz: Class[T]): T = {
    val instance = clazz.newInstance()
    if (map == null || map.isEmpty) return instance
    val fields = clazz.getDeclaredFields
    fields.foreach(field => {
      field.setAccessible(true)
      val name = field.getName
      map.get(name) match {
        case None =>
        case Some(value) if LocalDateTimeUtils.string2Time(value.toString) != null =>
          field.set(instance, LocalDateTimeUtils.string2Time(value.toString))
        case Some(value) if value.isInstanceOf[MapBean] =>
          field.set(instance, value.asInstanceOf[MapBean].toJsonString)
        case Some(value) =>
          field.set(instance, value)
      }
//      field.set(instance, map.get(name).orNull)
    })
    instance
  }

  def map2BeanList[T](mapList: List[MapBean], clazz: Class[T]): ListBuffer[T] = {
    val beanList = ListBuffer.empty[T]
    if (mapList == null || mapList.isEmpty) return beanList
    mapList.foreach(map => {
      val instance = map2Bean(map, clazz)
      if (instance != null) beanList += instance
    })
    beanList
  }

  def main(args: Array[String]): Unit = {
    val m = MapBean("s" -> ListBuffer("a"))
    System.out.println(m.getObject[ListBuffer[_]]("s"))
	  System.out.println(m.getList[String]("s"))
  }

}

@SerialVersionUID(8609134578238142L)
class MapBean extends mutable.HashMap[String, Any] {

  def get[T](key: String, defaultValue: T): T = {
    val v = get(key)
    if (v.isEmpty) defaultValue else v.get.asInstanceOf[T]
  }

  def getTime(key: String, defaultValue: Time): Time = get[Time](key, defaultValue)

  def getTimeStamp(key: String, defaultValue: Timestamp): Timestamp = get[Timestamp](key, defaultValue)

  def getInt(key: String): Int = getInt(key, 0)

  def getShort(key: String): Short = {
    val value = getInt( key )
    if (value < Short.MinValue || value > Short.MaxValue)
      throw new IllegalArgumentException(s"Short value:value out of range for short")

    value.toShort
  }

  def getInt(key: String, defaultValue: Int): Int = getNumber(key, defaultValue).intValue()

  def getNumber(key: String, defaultValue: Number): Number = get(key, defaultValue)

  def getFloat(key: String): Float = getFloat(key, 0f)

  def getFloat(key: String, defaultValue: Float): Float = getNumber(key, defaultValue).floatValue()

  def getLong(key: String): Long = getLong(key, 0L)

  def getLong(key: String, defaultValue: Long): Long = getNumber(key, defaultValue).longValue()

  def getDouble(key: String): Double = getDouble(key, 0d)

  def getDouble(key: String, defaultValue: Double): Double = getNumber(key, defaultValue).doubleValue()

  def getString(key: String): String = getString(key, null)

  def getString(key: String, defaultValue: String): String = get(key, defaultValue)

  def getBoolean(key: String): Boolean = get(key, false)

  def getList[T](key: String): List[T] = get(key, List.empty[T])

  def getMap(key: String): MapBean = get(key, MapBean.empty)

  def getObject[T](key: String): T = get(key, null.asInstanceOf[T])

  def getBytes(key: String): Array[Byte] = get(key, Array.empty[Byte])

  def getUtilDate(key: String): java.util.Date = get(key, null)

  def getSqlDate(key: String): java.sql.Date = get(key, null)

  def toJsonString: String = {
    try {
      MapBean.objectMapper.writeValueAsString(toJavaMap)
    } catch {
      case e: IOException => throw new RuntimeException(e)
    }
  }

  def toJavaMap: util.Map[String, Any] = convertJava(this).asInstanceOf[util.HashMap[String, Any]]

  implicit def cov[T](l: List[T]): ListBuffer[T] = {
    if (l == null) return null
	  new ListBuffer[T]() ++= l
  }

  implicit def cov[T](l: ListBuffer[T]): List[T] = {
    if (l == null) return null
	  l.toList
  }

  //对 value 进行类型匹配，将Scala 类型的转成Java类型
  private def convertJava(value: Any): Any = value match {
    case a: Array[_]@unchecked =>
      val list = new util.ArrayList[Any]()
      for (e <- a) {
        list.add(convertJava(e))
      }
      list
    case l: Seq[_]@unchecked =>
      val list = new util.ArrayList[Any]()
      for (e <- l) {
        list.add(convertJava(e))
      }
      list
    case s: scala.collection.Set[_]@unchecked =>
      val set = new util.HashSet[Any]()
      for (e <- s) {
        set.add(convertJava(e))
      }
      set
    case m: mutable.Map[String, _]@unchecked =>
      val map = new util.HashMap[String, Any]()
      for ((k, v) <- m) {
        map.put(k, convertJava(v))
      }
      map
    case m: Map[String, _]@unchecked =>
      val map = new util.HashMap[String, Any]()
      for ((k, v) <- m) {
        map.put(k, convertJava(v))
      }
      map
    case _ =>
      value
  }

  override def clone: MapBean = MapBean.toMutableMap(super.clone)
}
