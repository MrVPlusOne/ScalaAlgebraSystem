package core

/**
 * Created by weijiayi on 9/9/15.
 */
object Convenience {
  def sum(es: Expr*) = Expr.sum(es.toIndexedSeq)
  def times(es: Expr*) = Expr.times(es.toIndexedSeq)
  def int(v: Int) = NumInt.create(v)

  val x = Symb("x")
  val y = Symb("y")
  val z = Symb("z")

}
