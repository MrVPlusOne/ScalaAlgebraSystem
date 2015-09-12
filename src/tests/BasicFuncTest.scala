package tests

/**
 * Created by weijiayi on 9/9/15.
 */
class BasicFuncTest extends MyTest{
  "expr should match" in {
    import core._
    import NumRatio.ratio
    import core.Convenience._

    sum(times(x, ratio(4, 5), y, x, ratio(2, 10)), times(Power(x, int(2)), y)) ==> times(ratio(29, 25), y, Power(x, int(2)))
  }
}
