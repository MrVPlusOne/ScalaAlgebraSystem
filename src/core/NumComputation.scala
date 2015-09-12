package core

/**
 * This trait stores operators of Num
 */

trait NumComputation[T <: Num, D <: Num]{
  private def makeFunc(method: (T, T) => D) = (n1: Num, n2: Num) => {
    val a1 = n1.asInstanceOf[T]
    val a2 = n2.asInstanceOf[T]
    method(a1, a2)
  }

  val plus = makeFunc(plusMethod)
  val times = makeFunc(timesMethod)

  def negate(n: Num) = negateMethod(n.asInstanceOf[T])
  def inverse(n: Num) = inverseMethod(n.asInstanceOf[T])

  protected def plusMethod(n1: T, n2: T): D
  protected def timesMethod(n1: T, n2: T): D

  protected def inverseMethod(n: T): D
  protected def negateMethod(n: T): T

//  private def minusMethod(n1: T, n2: T) = plusMethod(n1, negateMethod(n2))
}

trait RealComputation[T <: NumReal] extends NumComputation[T, NumReal]

case class BinaryComputation[T<:Num, D<:Num](n1: Num, n2: Num, com: NumComputation[T,D]) {
  def plus() = com.plus(n1, n2)

  def times() = com.times(n1, n2)
}