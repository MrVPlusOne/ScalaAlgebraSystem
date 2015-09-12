package core

trait EvaluatingFunc extends Func{
  def evaled: Boolean

  def numHead = {
    assert(evaled)
    variables.head match{
      case n: Num => n
      case _ => NumOne
    }
  }

  def exprTail = {
    assert(evaled)
    variables.head match{
      case n: Num => variables.tail
      case _ => variables
    }
  }
}

case class Power protected(base: Expr, power: Expr) extends Func with ConcreteFunc {
  override def variables: IndexedSeq[Expr] = IndexedSeq(base, power)

  override def funcName: String = "^"
  override def prefixString: String = ""
  override def infixString: String = "^"

  override def evalFunc(env: EvalEnv, variables: IndexedSeq[Expr]): Expr = Power.create(variables.head, variables.last)

  override def derivativeAt(pos: Int): Expr = pos match{
    case 0 =>
      this * power / base
    case 1 =>
      this * SingleFunctions.log(base)
  }
}

object Power{
  def create(b: Expr, p: Expr): Expr = p match{
    case NumZero => NumOne
    case NumOne => b
    case _ => Power(b, p)
  }
}

case class Term(variables: IndexedSeq[Expr], evaled: Boolean) extends EvaluatingFunc with ConcreteFunc{
  override def infixString: String = "*"
  override def prefixString: String = ""
  override def funcName: String = "*"

  def evalFunc(env: EvalEnv, variables: IndexedSeq[Expr]): Expr = {
    import collection.mutable
    var num: Num = NumOne
    val termMap: mutable.Map[Expr, IndexedSeq[Expr]] = mutable.Map()

    def record(b: Expr, p: Expr) = {
      termMap.get(b) match {
        case Some(ps) => termMap(b) = ps :+ p
        case None => termMap(b) = IndexedSeq(p)
      }
    }

    def recordVariables(vs: Seq[Expr]): Boolean = {
      for(v <- vs) v match{
        case NumZero => return true //Any zero appears in the term will result zero
        case n: Num => num = num timesNum n
        case Power(b, p) => record(b, p)
        case Term(xs, _) => recordVariables(xs)
        case _ => record(v, NumOne)
      }
      false
    }

    if (recordVariables(variables)) return NumZero

    if(termMap.isEmpty) num
    else {
      val r = termMap.toVector.map { x =>
        val (b, ps) = x
        val p = Expr.sum(ps)
        Power.create(b, p)
      }

      val sorted = r.sortWith((e1, e2) => e1.isLowerExpr(e2))
      val contents = if(num == NumOne) sorted else num +: sorted
      if(contents.length>1)
        Term(contents, evaled = true)
      else contents.head
    }
  }

  override def derivativeAt(pos: Int): Expr = this / variables(pos)
}

case class Plus(variables: IndexedSeq[Expr], evaled: Boolean) extends EvaluatingFunc with ConcreteFunc{
  override def prefixString: String = ""
  override def infixString: String = "+"
  override def funcName: String = "+"

  def evalFunc(env: EvalEnv, variables: IndexedSeq[Expr]): Expr = {
    import collection.mutable
    type TermSeq = IndexedSeq[Expr]
    var num: Num = NumZero
    val termMap: mutable.Map[TermSeq, IndexedSeq[Expr]] = mutable.Map()

    def record(n: Expr, term: TermSeq) = {
      termMap.get(term) match {
        case Some(ns) => termMap(term) = ns :+ n
        case None => termMap(term) = IndexedSeq(n)
      }
    }

    for(v <- variables) v match{
      case n: Num => num = num plusNum  n
      case t: Term => record(t.numHead, t.exprTail)
      case _ => record(NumOne, IndexedSeq(v))
    }

    if(termMap.isEmpty) num
    else {
      val r = termMap.toVector.map { x =>
        val (term, ns) = x
        val n = Expr.sum(ns)
        n * Term(term, evaled = true)
      }

      val sorted = r.sortWith((e1, e2) => e1.isLowerExpr(e2))
      val contents = if(num == NumZero) sorted else num +: sorted
      if(contents.length>1)
        Plus(contents, evaled = true)
      else contents.head
    }
  }

  override def derivativeAt(pos: Int): Expr = NumOne
}
