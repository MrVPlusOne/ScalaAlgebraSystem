package core

/**
 * Created by weijiayi on 9/9/15.
 */
object SortExpr {
  def join[T](isLower: (T,T)=>Boolean)(x:T, xs:IndexedSeq[T]) = {
    xs.indexWhere(u=>isLower(x,u)) match {
      case -1 => xs :+ x
      case i =>
        val (h,t) = xs.splitAt(i)
        (h :+ x) ++  t
    }
  }

  val joinSymb = join[Symb]((s1, s2)=> s1.isLowerSymb(s2))_

  val joinFunc = join[Func]((f1, f2) => f1.isLowerFunc(f2))_
}
