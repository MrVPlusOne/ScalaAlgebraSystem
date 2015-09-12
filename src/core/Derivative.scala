package core

/**
 * Created by weijiayi on 9/9/15.
 */
case class Derivative(expr: Expr, s: Symb) extends SingleVariableFunc{
  override def arg: Expr = expr

  override def eval(env: EvalEnv, evaledArg: Expr): Expr = evaledArg match {
    case v: Symb => if (v == s) NumOne else NumZero
    case n: Num => NumZero
    case f: Func => f match {
      case f: ConcreteFunc =>
        Expr.sum(f.variables.indices.map(i =>
          Expr.times(IndexedSeq(f.derivativeAt(i), eval(env, f.variables(i))))
        )).evaluate(env)
      case _ => Derivative(f, s)
    }
  }
  override def funcName: String = "Dev"

  override def nicePrint: String = s"D[${s.nicePrint}](${expr.nicePrint})"
}
