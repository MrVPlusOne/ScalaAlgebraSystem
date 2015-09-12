package core

import core.Convenience._

/**
 * Created by weijiayi on 9/7/15.
 */
object Playground {
  import NumRatio.ratio
  import SingleFunctions._

  def evalPrintExpr(e:Expr) = println(e.evaluate().nicePrint)
  val iseq = IndexedSeq

  def main(args: Array[String]) {
    val env = new EvalEnv
    println {
      val ks = env.addUserFunc("ks", x, y)
      val yt = env.addUserFunc("yt", x, y)
      val u = env.addUserFunc("u", ks, yt)
      Derivative(Derivative(u, x),x).evaluate.nicePrint
    }

    println("where")
    println(env.envString)
  }
}
