package tests

/**
 * Created by weijiayi on 9/9/15.
 */
class SingleFuncTest extends MyTest{
  import core._
  import NumRatio.ratio
  import core.Convenience._
  import SingleFunctions._

  "NumDouble should make func evaluate to NumDouble" in {
    sin(NumInt(5)).evaluate() ==> sin(NumInt(5))
    sin(x).evaluate() ==> sin(x)
    sin(NumDouble(0)).evaluate() ==> NumDouble(0)
  }
}
