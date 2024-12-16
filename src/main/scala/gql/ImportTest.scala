package gql
import scala.collection.mutable

object ImportTest {
  val a=mutable.Set(1,2,3)
  val b=Set(1,2,3)

  trait Service
  def make()=new Service{
    def getId=123
  }

  val t: Service =make()
  print(t.getClass.getName)

  def main(args:Array[String]): Unit = {
    print(a.isInstanceOf[mutable.Set[_]])
    print(b.isInstanceOf[scala.collection.mutable.Set[_]])
    a.++(b)
    a ++ b
    val list=List(1,2,3)
    /**
     * 
     */
  }
}
