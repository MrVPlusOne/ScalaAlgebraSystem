package tests

/**
 * Created by weijiayi on 9/9/15.
 */
class NumericTests extends MyTest{
  import core._
  import NumRatio.ratio

  "some sample tests should pass" in {
    NumInt(3) plusNum ratio(5,3) plusNum ratio(2,6) minusNum NumInt(2) shouldBe NumInt(3)
    NumDouble(3.5) plusNum NumInt(2) shouldBe NumDouble(5.5)
    NumComplex(ratio(2,3),ratio(1,2)) timesNum NumComplex(ratio(1,3), ratio(7,4)) shouldBe NumComplex(ratio(47,-72),ratio(4,3))
  }
}
