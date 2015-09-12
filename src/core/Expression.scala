package core

/**
 * Created by weijiayi on 9/9/15.
 */
trait Expr {
  def nicePrint: String
  
  def isLowerExpr (e: Expr): Boolean

  def evaluate(env: EvalEnv): Expr

  def evaluate(): Expr = evaluate(EvalEnv.empty)

  def * (e1: Expr) = Expr.times(IndexedSeq(this, e1))

  def + (e1: Expr) = Expr.sum(IndexedSeq(this, e1))

  def / (e2: Expr) = this * e2.inverse

  def negate: Expr = NumInt.minus * this

  def inverse: Expr = Power.create(this, NumInt.minus)
}

object Expr{
  def sum(es: IndexedSeq[Expr]): Expr = Plus(es, evaled = false).evaluate()
  def times(es: IndexedSeq[Expr]): Expr = Term(es, evaled = false).evaluate()
}

case class Symb(name: String) extends Expr with UserArg{

  override def evaluate(env: EvalEnv): Expr = this

  override def nicePrint: String = name

  def isLowerSymb(symb: Symb) = name < symb.name

  override def isLowerExpr(e: Expr): Boolean = e match {
    case n: Num => false
    case s: Symb => isLowerSymb(s)
    case _ => true
  }

  override def meaning: Expr = this

  override def posName = this
}

trait Func extends Expr{
  def variables: IndexedSeq[Expr]

  def infixString: String = ","
  def prefixString: String

  override def nicePrint: String = s"$prefixString(${variables.map(_.nicePrint).mkString(infixString)})"

  def argNum = variables.length
  def funcName: String
  
  def isLowerFunc(f: Func): Boolean = {
    if(argNum == f.argNum) {
      if(funcName == f.funcName){
        for(i <- 0 until argNum){
          if(variables(i).isLowerExpr(f.variables(i)))
            return true
        }
        false
      }else funcName < f.funcName
    } else argNum < f.argNum
  }

  override def isLowerExpr(e: Expr): Boolean = e match {
    case f: Func => isLowerFunc(f)
    case _ => false
  }

  override def evaluate(env: EvalEnv): Expr = {
    val evaluated = variables.map(_.evaluate(env))
    evalFunc(env, evaluated)
  }

  protected def evalFunc(env: EvalEnv, variables: IndexedSeq[Expr]): Expr
}

trait ConcreteFunc {
  def derivativeAt(pos: Int): Expr
}
