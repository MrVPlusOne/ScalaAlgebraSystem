package core

case class UserFunc (name: String, posNames: IndexedSeq[Symb], variables: IndexedSeq[Expr], devs: IndexedSeq[Symb])
    extends Func with ConcreteFunc with UserArg{
  override def prefixString: String = ???

  override def funcName: String = name

  override def nicePrint: String =
    if(devs.isEmpty) name
    else s"$name[${devs.map(_.nicePrint).mkString(",")}]"

  override protected def evalFunc(env: EvalEnv, variables: IndexedSeq[Expr]): Expr =
    UserFunc(name, posNames, variables.map(_.evaluate(env)), devs)

  override def derivativeAt(pos: Int): Expr = UserFunc(name, posNames, variables, devs :+ posNames(pos))

  override def meaning: Expr = this
  
  override def posName = Symb(name)
}
