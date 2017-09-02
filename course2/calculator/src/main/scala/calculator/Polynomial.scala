package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var {
      val (aVal, bVal, cVal): (Double, Double, Double) = (a(), b(), c())

      math.pow(bVal, 2) - (4 * aVal * cVal)
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var {
      val (aVal, bVal, cVal): (Double, Double, Double) = (a(), b(), c())
      Set((-bVal + computeDelta(a, b, c)()) / (2 * aVal), (-bVal - computeDelta(a, b, c)()) / (2 * aVal))
    }
  }
}
