package core

object IntComputations extends RealComputation[NumInt]{
  override def plusMethod(n1: NumInt, n2: NumInt): NumInt = NumInt.create(n1.value + n2.value)

  override protected def timesMethod(n1: NumInt, n2: NumInt): NumInt = NumInt.create(n1.value * n2.value)

  override def negateMethod(n: NumInt): NumInt = NumInt.create(-n.value)

  override def inverseMethod(n: NumInt): NumReal = NumRatio.ratio(1,n.value)
}

object RatioComputations extends RealComputation[NumRatio]{
  import NumRatio.ratio
  override protected def plusMethod(n1: NumRatio, n2: NumRatio) =
    ratio(n1.n * n2.d + n2.n * n1.d, n1.d * n2.d)

  override protected def timesMethod(n1: NumRatio, n2: NumRatio) =
    ratio(n1.n * n2.n, n1.d * n2.d)

  override def negateMethod(n: NumRatio): NumRatio = NumRatio.create(-n.n, n.d)

  override def inverseMethod(n: NumRatio): NumReal = ratio(n.d, n.n)
}

object DoubleComputations extends RealComputation[NumDouble]{
  override protected def plusMethod(n1: NumDouble, n2: NumDouble): NumDouble =
    NumDouble(n1.value + n2.value)

  override protected def timesMethod(n1: NumDouble, n2: NumDouble): NumDouble =
    NumDouble(n1.value * n2.value)

  override def negateMethod(n: NumDouble): NumDouble = NumDouble(-n.value)

  override def inverseMethod(n: NumDouble): NumReal = NumDouble(1.0/n.value)
}

object ComplexComputations extends NumComputation[NumComplex, Num]{
  override protected def plusMethod(n1: NumComplex, n2: NumComplex): NumComplex =
    NumComplex(n1.re.plusReal(n2.re),n1.im.plusReal(n2.im))

  override protected def timesMethod(n1: NumComplex, n2: NumComplex): NumComplex =
    NumComplex((n1.re timesReal n2.re) minusReal (n1.im timesReal n2.im), (n1.re timesReal n2.im) plusReal (n1.im timesReal n2.re))

  override def negateMethod(n: NumComplex): NumComplex = NumComplex(n.re.negateReal, n.im.negateReal)

  override def inverseMethod(n: NumComplex): Num = n.conj timesReal n.norm.inverseReal
}