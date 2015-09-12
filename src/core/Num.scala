package core

trait Num extends Expr{
  def plusNum (other: Num): Num = Num.computeWith(this, other).plus()

  def timesNum (other: Num): Num = other match{
    case NumZero => NumZero.timesNum(this)
    case _ => Num.computeWith(this, other).times()
  }

  def divideNum (other: Num) = this timesNum other.inverseNum

  def minusNum (other: Num) = this plusNum other.negateNum

  def isComplex: Boolean

  def toComplex: NumComplex

  def generality: Generality.Value

  def computation: NumComputation[_<:Num, _<:Num]

  def inverseNum: Num = computation.inverse(this)

  def negateNum: Num = computation.negate(this)

  override def inverse = inverseNum

  override def negate = negateNum

  override def isLowerExpr(e: Expr): Boolean = e match {
    case n: Num => isLowerNum(n)
    case _ => true
  }

  def isLowerNum(n: Num): Boolean

  override def evaluate(env: EvalEnv): Expr = this
}

object Num {
  def computeWith(n1: Num, n2: Num) = {

    if(n1.isComplex || n2.isComplex){
      val a1 = n1.toComplex
      val a2 = n2.toComplex
      val g = Generality.max(a1, a2)
      BinaryComputation(a1.toGenerality(g), a2.toGenerality(g), ComplexComputations)
    }else{
      n1.asInstanceOf[NumReal].computeWith(n2.asInstanceOf[NumReal])
    }
  }
}

case class NumComplex(re: NumReal, im: NumReal) extends Num {
  assert(re.generality == im.generality)

  def toGenerality(g: Generality.Value) =
    if (g == generality) this
    else NumComplex(re.toGenerality(g), im.toGenerality(g))

  override def isComplex: Boolean = true

  override def toComplex: NumComplex = this

  override def generality: Generality.Value = re.generality
  
  def conj = NumComplex(re, im.negateReal)

  def timesReal(a: NumReal) = NumComplex(a.timesReal(re), a.timesReal(im))

  def norm = (re timesReal re) plusReal (im timesReal im)

  override def computation: NumComputation[_ <: Num, _ <: Num] = ComplexComputations

  override def nicePrint: String = s"$re+I* $im"

  override def isLowerNum(n: Num): Boolean = {
    if(n.isComplex){
      generality < n.generality
    }else{
      false
    }
  }
}

object NumZero extends NumInt(0){
  override def timesNum(other: Num): Num = NumZero

  override def timesReal(other: NumReal): NumReal = NumZero

  override def toString = "NumZero"
}
object NumOne extends NumInt(1){
  override def toString: String = "NumOne"
}