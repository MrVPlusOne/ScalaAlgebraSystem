package core

trait NumReal extends Num {
  def toGenerality(g: Generality.Value): NumReal

  protected def computationReal: RealComputation[_<:NumReal]

  def computeWith(other: NumReal) = {
    val (a1, a2) = NumReal.toSameGenerality(this, other)
    BinaryComputation(a1, a2, a1.computationReal)
  }

  def plusReal(other: NumReal): NumReal = computeWith(other).plus()

  def timesReal(other: NumReal) = computeWith(other).times()

  def minusReal(other: NumReal) = computeWith(other.negateReal).plus()

  def divideReal(other: NumReal) = computeWith(other.inverseReal).times()

  override def isComplex: Boolean = false

  override def toComplex: NumComplex = NumComplex(this, NumZero)

  def negateReal = computationReal.negate(this)

  def inverseReal = computationReal.inverse(this)

  override def computation: NumComputation[_<:Num, _<:Num] = computationReal

  override def isLowerNum(n: Num): Boolean = {
    if(n.isComplex) true
    else generality < n.generality
  }
}

object NumReal {
  def toSameGenerality(n1: NumReal, n2: NumReal) = {
    val g = Generality.max(n1, n2)
    (n1.toGenerality(g), n2.toGenerality(g))
  }

}


case class NumInt protected(value: Int) extends NumReal {
  override def generality: Generality.Value = Generality.numInt

  override def toGenerality(g: Generality.Value): NumReal = g match{
    case Generality.numInt => this
    case Generality.numRatio => NumRatio.create(value, 1)
    case Generality.`numDouble` => NumDouble(value.toDouble)
  }

  override def computationReal = IntComputations

  override def nicePrint: String = value.toString
}

object NumInt{
  def create(v: Int) = v match{
    case 0 => NumZero
    case 1 => NumOne
    case _ => NumInt(v)
  }

  val minus = NumInt(-1)
}


case class NumRatio private(n: Int, d: Int) extends NumReal {
  override def generality: Generality.Value = Generality.numRatio

  override def toGenerality(g: Generality.Value): NumReal = g match {
    case Generality.numRatio => this
    case Generality.`numDouble` => NumDouble(n.toDouble / d)
  }

  def reverse = NumRatio(d,n)

  override def computationReal = RatioComputations

  override def nicePrint: String = s"$n/$d"
}

object NumRatio{
  def gcd(a: Int, b: Int): Int =
    if(b==0) a else gcd(b, a%b)

  def ratio(n: Int, d: Int): NumReal = {
    if(n==0) NumZero
    else {
      val g = gcd(n, d)
      val (nn, dd) = (n / g, d / g)
      if (dd == 1) NumInt.create(nn)
      else NumRatio(nn, dd)
    }
  }

  def create(n: Int, d: Int) = NumRatio(n, d)
}


case class NumDouble(value: Double) extends NumReal {
  override def generality: Generality.Value = Generality.numDouble

  override def toGenerality(g: Generality.Value): NumReal = g match {
    case Generality.numDouble => this
  }

  override def computationReal = DoubleComputations

  override def nicePrint: String = value.toString
}


object Generality extends Enumeration {
  val numInt = Value
  val numRatio = Value
  val numDouble = Value

  def max(n1: Num, n2: Num) = {
    Generality(math.max(n1.generality.id, n2.generality.id))
  }
}
