package gql

class FuncQueue[T] private (val leading:List[T],val trailing:List[T]){
  def head: T =mirror.leading.head

  private def mirror={
    if (leading.isEmpty) new FuncQueue(trailing.reverse,Nil)
    else this
  }

  def tail: FuncQueue[T] ={
    val m=mirror
    new FuncQueue(m.leading.tail,m.trailing)
  }

  def append(x:T)=new FuncQueue(leading,x::trailing)
}

object FuncQueue{
  def apply[T](paras:T*)=new FuncQueue(paras.toList,Nil)
}
