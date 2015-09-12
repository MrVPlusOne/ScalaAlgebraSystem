package core

trait SingleVariableFunc extends Func {
  def arg: Expr

  override def evalFunc(env: EvalEnv, variables: IndexedSeq[Expr]): Expr = eval(env, arg.evaluate(env))

  override def prefixString: String = funcName

  override def variables: IndexedSeq[Expr] = IndexedSeq(arg)

  def eval(env: EvalEnv, evaledArg: Expr): Expr
}

trait NumFuncCore {
  def doubleNumFunc (d:NumDouble) = NumDouble(doubleFunc(d.value))
//  def complexNumFunc (c: NumComplex): NumComplex

  protected def doubleFunc: Double => Double

  def name: String

  def diff(arg: Expr): Expr
}

case class SingleNumericFunc(core: NumFuncCore, arg: Expr) extends SingleVariableFunc with ConcreteFunc{

  override def funcName: String = core.name

  override def eval(env: EvalEnv, arg: Expr): Expr = arg match {
    case d: NumDouble => core.doubleNumFunc(d)
//    case n: NumComplex if n.generality == Generality.numDouble => func.complexNumFunc(n)
    case _ => SingleNumericFunc(core, arg)
  }

  override def derivativeAt(pos: Int): Expr = core.diff(arg)
}

object SingleFunctions {

  def sin(arg: Expr) = SingleNumericFunc(SinFunc, arg)

  object SinFunc extends NumFuncCore{
    override protected def doubleFunc = math.sin

    override def name: String = "Sin"

    override def diff(arg: Expr) = cos(arg)
  }

  def cos(arg: Expr) = SingleNumericFunc(CosFunc, arg)

  object CosFunc extends NumFuncCore{
    override protected def doubleFunc = math.cos

    override def name: String = "Cos"

    override def diff(arg: Expr): Expr = sin(arg).negate
  }

  def exp(arg: Expr) = SingleNumericFunc(ExpFunc, arg)

  object ExpFunc extends NumFuncCore{
    override protected def doubleFunc = math.exp

    override def name: String = "Exp"

    override def diff(arg: Expr): Expr = exp(arg)
  }

  def log(arg: Expr) = SingleNumericFunc(LogFunc, arg)

  object LogFunc extends NumFuncCore{
    override protected def doubleFunc: (Double) => Double = math.log

    override def name: String = "Log"

    override def diff(arg: Expr): Expr = arg.inverse
  }
}