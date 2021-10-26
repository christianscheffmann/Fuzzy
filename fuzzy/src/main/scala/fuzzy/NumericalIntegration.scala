package fuzzy

object NumericalIntegration {
  def integrate(f: Double => Double, a: Double, b: Double, n: Int = 1000): Double = {
    val stepsize = (b - a) / n

    def integrateAgg(innera: Double, innerb: Double): Double = {
      val newa = innerb
      val newb = innerb + stepsize
      if (innerb > b) (b - innera) * 1 / 2 * (f(innera) + f(b))
      else (innerb - innera) * 1 / 2 * (f(innera) + f(innerb)) + integrateAgg(newa, newb)
    }

    integrateAgg(a, a + stepsize)
  }

  def centroid(f: Double => Double, a: Double, b: Double): Double = {
    val area = NumericalIntegration.integrate(f, a, b)
    val g: Double => Double = (x: Double) => x*f(x)
    1/area * NumericalIntegration.integrate(g, a, b)
  }
}