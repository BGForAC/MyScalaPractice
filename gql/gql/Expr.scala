package gql

import gql.Element.elem

sealed abstract class Expr
case class Var (name:String) extends Expr
case class Number (number:Double) extends Expr
case class UnOp(operator:String,arg:Expr) extends Expr
case class BinOp(operator:String,left:Expr,right: Expr) extends Expr

class ExprFormatter{

    def simplify(express:Expr): Expr = express match {
        case UnOp("-",UnOp("-",e))=>simplify(e)
        case BinOp("+",Number(0),e)=>simplify(e)
        case BinOp("*",Number(1),e)=>simplify(e)
        case Var(e)=>express
        case Number(e)=>express
    }

    private val onGroup = Array(
        Set("|","||"),
        Set("&","&&"),
        Set("^"),
        Set("==","!="),
        Set("<","<=",">",">="),
        Set("+","-"),
        Set("*","%")
    )

    private val precedence={
        val assoc=for (i<-onGroup.indices;op<-onGroup(i)) yield (op->i)
        Map.empty ++ assoc
    }

    val unaryPrecedence = onGroup.length
    val fractionPrecedence = -1

    private def format(e:Expr,enclPrec:Int): Element={
        e match {
            case Var(name)=>elem(name)

            case Number(num)=>{
                def destripDot(num:String) = {
                    if (num.endsWith(".0")) num.substring(0,num.length-2)
                    else num
                }
                elem(destripDot(num.toString))
            }

            case UnOp(op,arg)=>{
                elem(op) beside format(arg,unaryPrecedence)
            }

            case BinOp("/",left,right)=>{
                val top=format(left,fractionPrecedence)
                val bot=format(right,fractionPrecedence)
                val line=elem('-',top.width max bot.width,1)
                val oper=top above line above bot
                if (enclPrec!=fractionPrecedence) oper
                else elem(" ") beside oper beside elem(" ")
            }

            case BinOp(operator, left, right)=>{
                val opPrec=precedence(operator)
                val oper=format(left,opPrec) beside elem(" "+operator+" ") beside format(right,opPrec+1)
                if (enclPrec<=opPrec) oper
                else elem("(") beside oper beside elem(")")
            }
        }
    }

    def format(e: Expr): Element = elem(" ") beside format(e, 0) above elem(" ")
}

object start extends App{
    val f=new ExprFormatter
    val e1=BinOp("*",BinOp("/",Number(1),Number(2)),BinOp("+",Var("x"),Number(1)))
    val e2=BinOp("+",BinOp("/",Var("x"),Number(2)),BinOp("/",Number(1.5),Var("x")))
    val e3=BinOp("/",e1,e2)
    val e4=BinOp("/",BinOp("/",Number(1),Number(1)),Number(1))
    println(f.format(e1))
    println(f.format(e2))
    println(f.format(e3))
    println(f.format(e4))
    val e5=f.simplify(BinOp("+",Number(0),BinOp("*",Number(1),UnOp("-",UnOp("-",Var("x"))))))
    println(e5)
}

