package tests

import core.Expr
import org.scalatest.{ShouldMatchers, WordSpec}
import scala.language.implicitConversions

trait MyTest extends WordSpec with ShouldMatchers{
  implicit def expr2Assertion(ex: Expr): ExprAssertion = ExprAssertion(ex)
}

case class ExprAssertion(actual: Expr){
  def ==>(expected: Expr): Unit ={
    if(actual != expected) throw new Exception(s"${actual.nicePrint} didn't match ${expected.nicePrint}")
  }
}

